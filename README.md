# From forked 
> This is a fork of the emacs pastebin.com plugin avaialble via elpa.

> pastebin.com appear to have changed their API, so I have pathced the > pastebin.com file to reflect the new API.
> 
> Also new, you'll need your own API key. Create an account on 
> pastebin.com then go to http://www.pastebin.com/api to see what your > key is. It needs to replace the string GET_YOUR_OWN_API_KEY in 
> pastebin.el. I know that is ugly but it scratches my need. Feel free > to fork and fix that. :)

# Features 

- The name of the current buffer is used as title to pastes
- Get a list of your pastes in a interactive buffer using pastebin-list-buffer command
- Open a paste by pressing RET on it at this buffer
- Delete a paste by moving point to it and pressing `d'
- Replace a paste using the command pastebin-replace. If there is no paste with the name of the current buffer a new one is create. Note that the URL of the paste will change, just the name is keeped
- Refresh the list by pressing `r'

# Wishlist 
   
- Autoload major modes based on paste_format attribute while fetching a paste.

# TODO

- Docstring everthing
- Remove useless code (Review)
- Fix elpa stuff


