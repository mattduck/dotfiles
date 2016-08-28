--[[
My hammerspoon setup. Provides some tmux-like window manager bindings.
   
General features:
- change focus: hjkl np qQ
- reposition window: HJKL Zz ;
- launch/focus on apps: cmd
- toggle sticky mode: .

TODO:
- Disable modal keys that aren't bound?
  - Doing this manually atm.
- Preset layouts?
--]]


logger = hs.logger.new('md', 'debug')
logger.f('loading...')


-- Position variables.
positions = {
  maximized = hs.layout.maximized,

  left34 = {x=0, y=0, w=0.34, h=1},
  left50 = hs.layout.left50,
  left66 = {x=0, y=0, w=0.66, h=1},

  right34 = {x=0.66, y=0, w=0.34, h=1},
  right50 = hs.layout.right50,
  right66 = {x=0.34, y=0, w=0.66, h=1},

  bottom34 = {x=0, y=0.66, w=1, h=0.34},
  bottom50 = {x=0, y=0.50, w=1, h=0.50},
  bottom66 = {x=0, y=0.34, w=1, h=0.66},

  top34 = {x=0, y=0, w=1, h=0.34},
  top50 = {x=0, y=0, w=1, h=0.50},
  top66 = {x=0, y=0, w=1, h=0.66},
}


-- Window configuration.
hs.window.animationDuration = 0


-- The magic starts here. Modal ftw!
tmuxirl = hs.hotkey.modal.new({"ctrl"}, "`")


-- When in tmuxirl mode, highlight the focused window with a border.
function tmuxirl:entered()
  hs.window.highlight.ui.overlay = true
  hs.window.highlight.start()
end

function tmuxirl:exited()
  hs.window.highlight.stop()
end


-- Border styling.
hs.window.highlight.ui.overlayColor = {0.2, 0.05, 0, 0.01}
hs.window.highlight.ui.frameWidth = 3
hs.window.highlight.ui.frameColor = {1, 1, 0, 0.8}
hs.window.highlight.ui.overlay = false


-- "Sticky" feature. When enabled, remain inside tmuxirl
-- mode after commands are pressed. When disabled, exit after each command.
TMUXIRL_STICKY_MODE = false

function tmuxirl:stickyToggle ()
  if TMUXIRL_STICKY_MODE then
    TMUXIRL_STICKY_MODE = false
    hs.alert('STICKY: disabled')
  else
    TMUXIRL_STICKY_MODE = true
    hs.alert('STICKY: enabled')
  end
end

function tmuxirl:stickyExit (force)
  if TMUXIRL_STICKY_MODE and not force then
    return
  end
  TMUXIRL_STICKY_MODE = false
  tmuxirl:exit() 
end


-- sticky / exit keys
tmuxirl:bind('', '.', function () tmuxirl:stickyToggle() end)
tmuxirl:bind('', "escape", function () tmuxirl:stickyExit(true) end)
tmuxirl:bind('', "return", function () tmuxirl:stickyExit(true) end)
tmuxirl:bind('ctrl', "g", function () tmuxirl:stickyExit(true) end)
tmuxirl:bind('ctrl', "`", function () tmuxirl:stickyExit(true) end)


-- hjkl move focus to another window
movement_bindings = {
   {key="h", direction="west"},
   {key="j", direction="south"},
   {key="k", direction="north"},
   {key="l", direction="east"},
}
hs.fnutils.each(movement_bindings, function(entry)
  tmuxirl:bind('', entry.key, function ()
  local fn = "focusWindow" .. (entry.direction:gsub("^%l", string.upper))
  local win = hs.window.focusedWindow()
  if win then
    win[fn]()
  end
    tmuxirl:stickyExit()
  end)
end)


-- HJKL to reposition the current window.
-- Repeat to cycle through the size variations.
grid = {
  {key="h", units={positions.left50, positions.left66, positions.left34}},
  {key="j", units={positions.bottom50, positions.bottom66, positions.bottom34}},
  {key="k", units={positions.top50, positions.top66, positions.top34}},
  {key="l", units={positions.right50, positions.right66, positions.right34}},
}
gridIndex = {}
hs.fnutils.each(grid, function(entry)
  tmuxirl:bind({'shift'}, entry.key, function()
    local units = entry.units
    local window = hs.window.focusedWindow()
    local index = gridIndex[entry.key]

    if index == nil then index = 0 end
    if index == #units then index = 0 end
    window:moveToUnit(units[index + 1])
    index = index + 1
    gridIndex[entry.key] = index
    tmuxirl:stickyExit()
  end)
end)


-- ; to move current window to another screen.
-- repeat to cycle through screens.
screenIndex = 1
tmuxirl:bind('shift', ";", function ()
  local screens = hs.screen.allScreens()
  local win = hs.window.focusedWindow()

  if screenIndex == #screens then screenIndex = 0 end
  win:moveToScreen(screens[screenIndex + 1])
  screenIndex = screenIndex + 1
  tmuxirl:stickyExit()
end)


-- n and p to visit next/previous spaces.
-- AFAICT Hammerspoon doesn't officially support a spaces API,
-- so I'm achieving this by executing the default OS X
-- shortcuts.
tmuxirl:bind('', "n", function ()
  hs.eventtap.keyStroke({"ctrl"}, "right")
  tmuxirl:stickyExit()
end)
tmuxirl:bind('', "p", function ()
  hs.eventtap.keyStroke({"ctrl"}, "left")
  tmuxirl:stickyExit()
end)


-- For repositioning the current window to the next/previous space,
-- we have to go one step further: there don't seem to be
-- official OS X bindings for this, so these correspond
-- to bindings that I've setup in BetterTouchTool.
tmuxirl:bind('shift', "n", function ()
  hs.eventtap.keyStroke({"ctrl", "alt", "cmd"}, "right")
  tmuxirl:stickyExit()
end)
tmuxirl:bind('shift', "p", function ()
  hs.eventtap.keyStroke({"ctrl", "alt", "cmd"}, "left")
  tmuxirl:stickyExit()
end)
                                   

-- Z to toggle fullscreen...
tmuxirl:bind('shift', "z", function ()
  local win = hs.window.focusedWindow()
  win:toggleFullScreen()
  tmuxirl:stickyExit()
end)


--- ...and 'z' toggle maximising the window
center_toggle = {}
tmuxirl:bind('', "z", function ()
  local win = hs.window.focusedWindow()
  if win and center_toggle[win:id()] then
    win:setFrame(center_toggle[win:id()])
    center_toggle[win:id()] = nil
  else
    center_toggle[win:id()] = win:frame()
    win:moveToUnit(positions.maximized)
  end
  tmuxirl:stickyExit()
end)


-- Expose Styling - used by 'Q' below
  -- Show everything
hs.expose.ui.includeNonVisible=True
hs.expose.ui.onlyActiveApplications=False

  -- Change styling to try to reduce overlaps appearing
hs.expose.ui.showTitles=False
hs.expose.ui.showThumbnails=False
hs.expose.ui.textSize=30

  -- More screen estate for other spaces
hs.expose.ui.nonVisibleStripWidth=0.3

  -- This is meant to improve responsiveness of this command,
  -- but could decrease overall performance. I'll try it.
hs.expose.ui.fitWindowsInBackground=True


-- q and Q to select windows - similar to "q" in tmux.
-- The expose feature is experimental, although the
-- documentation doesn't warn about it as much as some of the other
-- modules, so hopefully it won't change much.
expose = hs.expose.new(nil)
tmuxirl:bind('shift', "q", function ()
  expose:show()
  tmuxirl:stickyExit()
end)

hs.hints.hintChars = {"j", "f", "k", "d", "l", "s", "a"}
tmuxirl:bind('', "q", function ()
  hs.hints.windowHints()
  tmuxirl:stickyExit()
end)


-- Launch individual application
launch_bindings = {
   {key="e", app="Emacs"},
   {key="i", app="iTerm"},
   {key="f", app="Firefox"},
   {key="s", app="Slack"},
}
hs.fnutils.each(launch_bindings, function(entry)
  tmuxirl:bind('cmd', entry.key, function ()
    hs.application.launchOrFocus(entry.app)
    tmuxirl:stickyExit()
  end)
end)


-- For safety, disable some keys that I dn't want to pass through.
-- I'd like it if all keys were disabled, but don't think this is a feature.
nil_chars = {"`", "'", "[", "]", ",", "\\", "i", "e", "f", "s", "o", "r", "t"}
hs.fnutils.each(nil_chars, function(nil_char)
  tmuxirl:bind('', nil_char, function () return end)
end)

hs.alert("Hammerspoon!", 0.5)
logger.f('...done.')
