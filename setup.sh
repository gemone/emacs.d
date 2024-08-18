# For Linux
python3 -m venv ./etc/python-venv
source ./etc/python-venv/bin/activate

git config --global core.autocrlf input

pip3 install -U ruff-lsp pyright jupyterlab jupyter-console

[[ -e `which npm` ]] && npm install --prefix ./etc/npm bash-language-server

[[ -e `which npm` ]] && npm install --prefix ./etc/npm typescript-language-server typescript
