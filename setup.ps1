python3 -m venv ./etc/python-venv
./etc/python-venv/Scripts/Activate.ps1

mkdir ./etc/bin

git config --global core.autocrlf true

pip3 install -U --trusted-host mirrors.huaweicloud.com -i https://mirrors.huaweicloud.com/repository/pypi/simple ruff-lsp pyright jupyterlab jupyter-console

if (Test-Path (where.exe npm)) {
  npm install --prefix --prefix ./etc/npm --registry=https://registry.npmmirror.com bash-language-server
}

if (Test-Path (where.exe npm)) {
  npm install --prefix --prefix ./etc/npm --registry=https://registry.npmmirror.com typescript-language-server typescript
}

function Install-ZLS {
    $dowload_path='.cache/download'
    del $download_path -Recurse -Force -Confirm:$false
    mkdir $download_path
    if (Test-Path (where.exe zig)) {
	$bin_path="./etc/bin"
	$var_path="./var/zls"
	del $var_path -Recurse -Force -Confirm:$false
	del $bin_path/zls.exe -Recurse -Force -Confirm:$false
	mkdir $var_path

	$arch=Switch ([intptr]::Size) {
            4 {"x86"}
            8 {"x86_64"}      
	}
	$version=(zig version)
	$url="https://github.com/zigtools/zls/releases/download/$version/zls-$arch-windows.zip"
	$dest="./.cache/download/zls.zip"
	Invoke-WebRequest -Uri $url -OutFile $dest
	Expand-Archive $dest -DestinationPath $var_path -Force
	mv $var_path/zls.exe $bin_path
    }
    del $download_path -Recurse -Force -Confirm:$false
}

if (Test-Path (where.exe npm)) {
  npm install --prefix --prefix ./etc/npm --registry=https://registry.npmmirror.com vscode-langservers-extracted
}
