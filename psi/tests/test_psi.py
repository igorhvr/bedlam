import os
import subprocess
import tempfile
import json
import base64
import pytest

ROOT = os.path.dirname(os.path.dirname(__file__))
PREP = os.path.join(ROOT, 'prepare_intersection.py')
FIND = os.path.join(ROOT, 'find_intersection.py')


def run(cmd):
    subprocess.check_call(cmd, shell=True)


def log_size(label, path):
    size = os.path.getsize(path)
    print(f"{label}: {size} bytes")


def log_key_size(state_path):
    data = json.loads(open(state_path, "r", encoding="utf-8").read())
    key_bytes = base64.b64decode(data["priv"])
    print(f"{state_path.name} key size: {len(key_bytes)} bytes")


@pytest.mark.parametrize("ds", ["gcs", "raw"])
def test_end_to_end(tmp_path, ds):
    server_list = tmp_path / 'server.txt'
    server_list.write_text('apple\nbanana\ncarrot\n')
    client_list = tmp_path / 'client.txt'
    client_list.write_text('banana\ndate\n')

    run(
        f"{PREP} --role server --input {server_list} --client-size 2 --state {tmp_path/'s_state.json'} --out {tmp_path/'setup.txt'} --ds {ds}"
    )
    log_size("server_setup.txt", tmp_path / 'setup.txt')
    log_size("server_state.json", tmp_path / 's_state.json')
    log_key_size(tmp_path / 's_state.json')
    run(f"{PREP} --role client --input {client_list} --state {tmp_path/'c_state.json'} --out {tmp_path/'request.txt'}")
    log_size("client_request.txt", tmp_path / 'request.txt')
    log_size("client_state.json", tmp_path / 'c_state.json')
    log_key_size(tmp_path / 'c_state.json')

    run(f"{FIND} --role server --state {tmp_path/'s_state.json'} --inblob {tmp_path/'request.txt'} --out {tmp_path/'response.txt'}")
    log_size("server_response.txt", tmp_path / 'response.txt')
    run(f"{FIND} --role client --state {tmp_path/'c_state.json'} --inblob {tmp_path/'setup.txt'} --response {tmp_path/'response.txt'} --input {client_list} --out {tmp_path/'out.txt'}")
    log_size("out.txt", tmp_path / 'out.txt')

    assert tmp_path.joinpath('out.txt').read_text().strip() == 'banana'
