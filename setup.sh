# For Linux
python3 -m venv ./etc/python-venv
source ./etc/python-venv/bin/activate

mkdir ./etc/bin

git config --global core.autocrlf input

pip3 install -U --trusted-host mirrors.huaweicloud.com -i https://mirrors.huaweicloud.com/repository/pypi/simple ruff-lsp pyright jupyterlab jupyter-console

[[ -e `which npm` ]] && npm install --global --prefix ./etc/npm --registry=https://registry.npmmirror.com bash-language-server

[[ -e `which npm` ]] && npm install --global --prefix ./etc/npm --registry=https://registry.npmmirror.com typescript-language-server typescript eslint

function install_zls() {
    [[ -e `which zig` ]] || return
    [[ ! -d .cache/download ]] && mkdir .cache/download -p
    download_file=.cache/download/zls.tar.xz
    bin_path=./etc/bin/

    current_arch=$(uname -m)
    current_system=$(uname -s)
    version=$(zig version)
    
    # I don't known how to define macos system. sorry.
    # please use bash to execute
    wget https://github.com/zigtools/zls/releases/download/${version}/zls-${current_arch,,}-${current_system,,}.tar.xz -O ${download_file}
    tar xvf ${download_file} --directory ${bin_path}
    chmod +x ${bin_path}/zls
    rm -rf ${download_file}
    mkdir -p ./var/zls
    mv ${bin_path}/{LICENSE,README.md} ./var/zls
}

install_zls

[[ -e `which npm` ]] && npm install --global --prefix ./etc/npm --registry=https://registry.npmmirror.com vscode-langservers-extracted
