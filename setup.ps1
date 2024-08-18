python3 -m venv ./etc/python-venv
./etc/python-venv/Scripts/Activate.ps1

git config --global core.autocrlf true

pip3 install -U ruff-lsp pyright jupyterlab jupyter-console

if (Test-Path (where.exe npm)) {
  npm install --prefix ./etc/npm bash-language-server
}

if (Test-Path (where.exe npm)) {
  npm install --prefix ./etc/npm typescript-language-server typescript
}
