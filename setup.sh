git config --global core.autocrlf input

# For Linux
python3 -m venv ./etc/python-venv
pip3 install -U readline
source ./etc/python-venv/bin/activate

pip3 install -U ruff-lsp
pip3 install -U pyright
pip3 install -U jupyterlab
pip3 install -U jupyter-console
