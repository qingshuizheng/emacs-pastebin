# From forked 
> This is a fork of the emacs pastebin.com plugin avaialble via elpa.

> pastebin.com appear to have changed their API, so I have pathced the > pastebin.com file to reflect the new API.
> 
> Also new, you'll need your own API key. Create an account on 
> pastebin.com then go to http://www.pastebin.com/api to see what your > key is. It needs to replace the string GET_YOUR_OWN_API_KEY in 
> pastebin.el. I know that is ugly but it scratches my need. Feel free > to fork and fix that. :)

# Wanted features

1. New pastes with pastebin-new
2. Replace pastes with pastebin-replace (creates a new if none exist)
3. Get a buffer with a list of pastes and open one by pressing pointing to it and pressing RET
4. Delete pastes
5. Trigger a minor mode on buffer resulting of step 2
