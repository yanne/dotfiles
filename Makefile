EMACSD=.doom.d
OH_MY_ZSH=.oh-my-zsh
ZSHRC=.zshrc
PYTHONRC=.pythonrc
TMUX=.tmux.conf
GITCONFIG=.gitconfig

all:
	links

clean:
	rm -rf $$HOME/{$(GITCONFIG),$(EMACSD),$(OH_MY_ZSH),$(ZSHRC),$(PYTHONRC),$(TMUX)}

links:
	git submodule init && git submodule update
	for p in $(GITCONFIG) $(EMACSD) $(OH_MY_ZSH) $(ZSHRC) $(PYTHONRC), $(TMUX); do \
		ln -s $$HOME/dotfiles/$$p $$HOME/ ; \
	done
	ln -s $$HOME/dotfiles/yanne.zsh-theme $$HOME/$(OH_MY_ZSH)/themes
