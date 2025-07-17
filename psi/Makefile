.PHONY: install test

install:
	uv pip install --system -r requirements.txt

test: install
	pytest -vv -s
