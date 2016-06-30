dotfiles
========

My dotfiles and assorted other crap, managed by [GNU Stow][]. For a
very good blog post on why Stow to manage dotfiles, see
[here][invergo].

[GNU Stow]: https://www.gnu.org/software/stow/
[invergo]: http://brandon.invergo.net/news/2012-05-26-using-gnu-stow-to-manage-your-dotfiles.html

How It Works
------------

The `stow` command creates symlinks for files in the parent directory
of where you execute the command, so this setup assumes that the repo
is located in your home directory (for example, `~/dotfiles`), and all
`stow` commands should be executed in that directory.

If that isn't the case, you can use `stow -d` to specify the repo
directory location.

To install my **zsh** settings, use the command:

```bash
stow zsh
```

This will symlink files to `~/.zshrc`, `~/.zsh`, and so on.

You can remove things with `stow -D`.

tl;dr
-----

```bash
cd ~

git clone https://github.com/barrucadu/dotfiles.git

cd dotfiles

stow zsh
stow git
...
```
