import os
import subprocess
import tempfile
import logging
import json
import base64
from pathlib import Path
import pytest

ROOT = os.path.dirname(os.path.dirname(__file__))
PREP = os.path.join(ROOT, 'prepare_intersection.py')
FIND = os.path.join(ROOT, 'find_intersection.py')

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def log_size(path):
    size = os.path.getsize(path)
    message = f"{os.path.basename(path)} size: {size} bytes"
    print(message)
    logger.info(message)


def log_key_size(path):
    with open(path, "r", encoding="utf-8") as f:
        data = json.load(f)
    key_bytes = base64.b64decode(data["priv"])
    message = f"{os.path.basename(path)} key size: {len(key_bytes)} bytes"
    print(message)
    logger.info(message)


def run(cmd):
    subprocess.check_call(cmd, shell=True)


@pytest.mark.parametrize("ds", ["gcs", "raw"])
def test_end_to_end(tmp_path, ds):
    server_list = tmp_path / 'server.txt'
    server_words = (Path(__file__).with_name('sampleWordListA.txt').read_text().strip().split("\n"))
    assert len(server_words) == 256
    server_list.write_text("\n".join(server_words) + "\n")
    log_size(server_list)
    client_list = tmp_path / 'client.txt'
    client_words = (Path(__file__).with_name('sampleWordListB.txt').read_text().strip().split("\n"))
    assert len(client_words) == 256
    client_list.write_text("\n".join(client_words) + "\n")
    log_size(client_list)

    run(
        f"{PREP} --role server --input {server_list} --client-size {len(client_words)} --state {tmp_path/'s_state.json'} --out {tmp_path/'setup.txt'} --ds {ds}"
    )
    log_size(tmp_path / 's_state.json')
    log_key_size(tmp_path / 's_state.json')
    log_size(tmp_path / 'setup.txt')
    run(
        f"{PREP} --role client --input {client_list} --state {tmp_path/'c_state.json'} --out {tmp_path/'request.txt'}"
    )
    log_size(tmp_path / 'c_state.json')
    log_key_size(tmp_path / 'c_state.json')
    log_size(tmp_path / 'request.txt')

    run(
        f"{FIND} --role server --state {tmp_path/'s_state.json'} --inblob {tmp_path/'request.txt'} --out {tmp_path/'response.txt'}"
    )
    log_size(tmp_path / 'response.txt')
    run(
        f"{FIND} --role client --state {tmp_path/'c_state.json'} --inblob {tmp_path/'setup.txt'} --response {tmp_path/'response.txt'} --input {client_list} --out {tmp_path/'out.txt'}"
    )
    log_size(tmp_path / 'out.txt')

    expected_set = set(server_words) & set(client_words)
    result_lines = tmp_path.joinpath('out.txt').read_text().strip().split("\n")
    assert set(result_lines) == expected_set

