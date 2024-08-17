git config --global core.autocrlf true

python3 -m venv ./etc/python-venv
./etc/python-venv/Scripts/Activate.ps1

pip3 install -U ruff-lsp
pip3 install -U pyright
