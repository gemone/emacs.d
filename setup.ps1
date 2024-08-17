git config --global core.autocrlf true

python3 -m venv ./etc/python-venv
pip3 install -U readline
./etc/python-venv/Scripts/Activate.ps1

pip3 install -U ruff-lsp
pip3 install -U pyright
pip3 install -U jupyterlab
pip3 install -U jupyter-console
