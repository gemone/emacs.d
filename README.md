
# .emacs.d

## `lexical-binding: t` 与文件头（File Header）

所有 `.el` 文件首行采用标准文件头：

```elisp
;;; FILENAME.el --- ONE-LINE-DESCRIPTION -*- lexical-binding: t; -*-
```

这是 Emacs 官方约定的文件头格式，三部分各有作用：

- **`;;; FILENAME.el`**：库注释起手式。两个以上 `;` 是「文件级注释」，按惯例首行写文件名，便于 `lispdir`、`finder` 等工具识别。
- **`--- ONE-LINE-DESCRIPTION`**：一行摘要。`lisp-mnt`（Emacs 的 Lisp 注释维护工具）以 ` --- ` 为分隔符提取它，作为该文件的简要说明（出现在 `C-h P` 包查找、`finder-commentary` 等输出里）。
- **`-*- lexical-binding: t; -*-`**：file-local 变量块（又称「魔法注释 / file variable header」）。Emacs 在加载文件前先扫描首行（及末尾的 `Local Variables:` 块）解析其中的 `-*- ... -*-`，把列出的变量设为 buffer-local，并对该文件切换到**词法作用域**。

为什么要显式声明：

- Emacs 24+ 推荐词法绑定；不声明的 `.el` 默认走动态绑定，并触发 `Missing lexical-binding` 编译警告。
- 词法绑定下闭包正确捕获变量，`defcustom` 中的 lambda、第三方包里的闭包行为才符合预期。
- `M-x customize` 写回 `custom.el` 时会保留这个 file-local 声明，加一次即可长期生效。

> 注：用首行的 `-*- -*-` 形式而非文件末尾的 `Local Variables:` 块，因为 `customize` 写回时对头部 cookie 的保留更可靠。

## `custom.el` 说明

`custom.el` 是机器本地配置（已 gitignore），由 `init.el` 在加载主题之前 `load` 进来，存放 `magit-git-executable`、代理等因机器/环境而异的设置。

## Git 配置

`git` 本身配置:

```shell
git config --global core.preloadindex true
git config --global core.fscache true
git config --global gc.auto 256
```

`Windows` 上的 `Magit` 配置，使用 `custom.el`:

```elisp
(custom-set-variables
 '(magit-git-executable "C:/Program Files/Git/bin/git.exe"))
```

## 代理配置

`custom.el` 里配置 HTTP/HTTPS 代理（`url-proxy-services`，elpaca 等网络包都走它）：

```elisp
(custom-set-variables
 '(url-proxy-services
   '(("http"     . "127.0.0.1:7890")
     ("https"    . "127.0.0.1:7890")
     ("no_proxy" . "^\\(localhost\\|192\\.168\\..*\\|10\\..*\\)"))))
```

- `"http"` / `"https"`: 代理地址，格式 `host:port`（按你本地代理改）
- `"no_proxy"`: 不走代理的地址（正则）
- 留空 `("http" . "")` 表示该协议不走代理；整个变量设为 `nil` 关闭代理
