# For Linux
python3 -m venv ./etc/python-venv
source ./etc/python-venv/bin/activate
pip3 install -U epc orjson sexpdata six setuptools paramiko rapidfuzz

pip3 install -U jedi-language-server
pip3 install -U ruff-lsp
