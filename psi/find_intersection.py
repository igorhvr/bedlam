#!/usr/bin/env python3
"""Finalize PSI protocol."""
import argparse
import base64
import json
from typing import List

import private_set_intersection.python as psi


def read_list(path: str) -> List[str]:
    with open(path, "r", encoding="utf-8") as f:
        return [line.strip() for line in f if line.strip()]


def main() -> None:
    parser = argparse.ArgumentParser(description="Process PSI messages")
    parser.add_argument("--role", choices=["server", "client"], required=True,
                        help="Run as server or client")
    parser.add_argument("--state", required=True, help="State file from prepare step")
    parser.add_argument("--inblob", required=True, help="Blob from other party")
    parser.add_argument("--out", required=True, help="Output file")
    parser.add_argument("--response", help="Server response blob (required for client role)")
    parser.add_argument("--input", help="Client list file (required for client role)")
    args = parser.parse_args()

    state = json.load(open(args.state, "r", encoding="utf-8"))
    priv = base64.b64decode(state["priv"])
    other_bytes = base64.b64decode(open(args.inblob, "r", encoding="utf-8").read())

    if args.role == "server":
        srv = psi.server.CreateFromKey(priv, reveal_intersection=True)
        req = psi.Request()
        req.ParseFromString(other_bytes)
        resp = srv.ProcessRequest(req)
        with open(args.out, "w", encoding="utf-8") as f:
            f.write(base64.b64encode(resp.SerializeToString()).decode())
    else:
        if not args.response or not args.input:
            parser.error("--response and --input are required for client role")
        cli = psi.client.CreateFromKey(priv, reveal_intersection=True)
        setup = psi.ServerSetup()
        setup.ParseFromString(other_bytes)
        resp_bytes = base64.b64decode(open(args.response, "r", encoding="utf-8").read())
        resp = psi.Response()
        resp.ParseFromString(resp_bytes)
        items = read_list(args.input)
        idxs = cli.GetIntersection(setup, resp)
        with open(args.out, "w", encoding="utf-8") as f:
            for i in idxs:
                f.write(items[i] + "\n")


if __name__ == "__main__":
    main()
