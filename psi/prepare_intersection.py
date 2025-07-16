#!/usr/bin/env python3
"""Prepare PSI messages for server or client role."""
import argparse
import base64
import json
from typing import List

import private_set_intersection.python as psi


def read_list(path: str) -> List[str]:
    with open(path, "r", encoding="utf-8") as f:
        return [line.strip() for line in f if line.strip()]


def main() -> None:
    parser = argparse.ArgumentParser(description="Prepare PSI blobs for exchange")
    parser.add_argument("--role", choices=["server", "client"], required=True,
                        help="Run as server or client")
    parser.add_argument("--input", required=True, help="Path to newline separated values")
    parser.add_argument("--state", required=True, help="File to store local state")
    parser.add_argument("--out", required=True, help="Blob to share with the other party")
    parser.add_argument("--client-size", type=int, help="Size of client set (required for server)")
    parser.add_argument("--fpr", type=float, default=1e-6, help="False positive rate for server setup")

    args = parser.parse_args()
    items = read_list(args.input)

    if args.role == "client":
        client = psi.client.CreateWithNewKey(reveal_intersection=True)
        request = client.CreateRequest(items)
        blob = base64.b64encode(request.SerializeToString()).decode()
        state = {
            "role": "client",
            "priv": base64.b64encode(client.GetPrivateKeyBytes()).decode(),
        }
    else:
        if args.client_size is None:
            parser.error("--client-size is required for server role")
        server = psi.server.CreateWithNewKey(reveal_intersection=True)
        setup = server.CreateSetupMessage(
            args.fpr, args.client_size, items
        )
        blob = base64.b64encode(setup.SerializeToString()).decode()
        state = {
            "role": "server",
            "priv": base64.b64encode(server.GetPrivateKeyBytes()).decode(),
            "client_size": args.client_size,
            "fpr": args.fpr,
        }

    with open(args.state, "w", encoding="utf-8") as f:
        json.dump(state, f)

    with open(args.out, "w", encoding="utf-8") as f:
        f.write(blob)


if __name__ == "__main__":
    main()
