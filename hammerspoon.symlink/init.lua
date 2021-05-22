--- Create a global keybinding to use Emacs as an alfred-style launcher / tool
logger = hs.logger.new('md', 'debug')
logger.f('Hammerspoon loading')

function alfred()
   hs.execute("/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -ne '(md/alfred)'")
end

hs.hotkey.bind({"cmd", "shift"}, "space", alfred)

logger.f('Hammerspoon loaded')
