import os
import subprocess
import tempfile
import pytest

ROOT = os.path.dirname(os.path.dirname(__file__))
PREP = os.path.join(ROOT, 'prepare_intersection.py')
FIND = os.path.join(ROOT, 'find_intersection.py')


def run(cmd):
    subprocess.check_call(cmd, shell=True)


@pytest.mark.parametrize("ds", ["gcs", "raw"])
def test_end_to_end(tmp_path, ds):
    server_list = tmp_path / 'server.txt'
    server_list.write_text('apple\nbanana\ncarrot\n')
    client_list = tmp_path / 'client.txt'
    client_list.write_text('banana\ndate\n')

    run(
        f"{PREP} --role server --input {server_list} --client-size 2 --state {tmp_path/'s_state.json'} --out {tmp_path/'setup.txt'} --ds {ds}"
    )
    run(f"{PREP} --role client --input {client_list} --state {tmp_path/'c_state.json'} --out {tmp_path/'request.txt'}")

    run(f"{FIND} --role server --state {tmp_path/'s_state.json'} --inblob {tmp_path/'request.txt'} --out {tmp_path/'response.txt'}")
    run(f"{FIND} --role client --state {tmp_path/'c_state.json'} --inblob {tmp_path/'setup.txt'} --response {tmp_path/'response.txt'} --input {client_list} --out {tmp_path/'out.txt'}")

    assert tmp_path.joinpath('out.txt').read_text().strip() == 'banana'
