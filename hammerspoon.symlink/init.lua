--[[
My hammerspoon setup. Provides some tmux-like window manager bindings.
   
External dependencies:
- cmd+alt+ctrl left / right: BTT move to next / previous space.
- cmd+alt+ctrl+shift left/right: BTT fullscreen left/right.
   
General features:
- change focus: hjkl o np qQ
- reposition window: HJKL Zz ; {}
- launch/focus on apps: cmd
- toggle sticky mode: .

TODO:
- a better way to resize the fullscreen split.
- the "hints" feature - any way to allow the iterm titleless windows?
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
hs.window.setFrameCorrectness = false  -- false by default. I set to true but seems slow, trying false again


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


-- Sticky / exit feature. When enabled, remain inside tmuxirl
-- mode after commands are pressed. When disabled, exit after each command.
TmuxIRLStickyMode = false

function tmuxirl:stickyToggle ()
  if TmuxIRLStickyMode then
    TmuxIRLStickyMode = false
    hs.alert('STICKY: Disabled')
  else
    TmuxIRLStickyMode = true
    hs.alert('STICKY: Enabled')
  end
end

function tmuxirl:exitUnlessStickyMode ()
   if TmuxIRLStickyMode then
      return
   end
   tmuxirl:forceExit() 
end

function tmuxirl:forceExit ()
   TmuxIRLStickyMode = false
   tmuxirl:exit()
end

tmuxirl:bind('', '.', tmuxirl.stickyToggle)
tmuxirl:bind('', "escape", tmuxirl.forceExit)
tmuxirl:bind('', "return", tmuxirl.forceExit)
tmuxirl:bind('ctrl', "g", tmuxirl.forceExit)
tmuxirl:bind('ctrl', "`", tmuxirl.forceExit)


-- hjkl move focus to another window
movementBindings = {
   {key="h", direction="west"},
   {key="j", direction="south"},
   {key="k", direction="north"},
   {key="l", direction="east"},
}
hs.fnutils.each(movementBindings, function(entry)
  tmuxirl:bind('', entry.key, function ()
    local focusFn = "focusWindow" .. (entry.direction:gsub("^%l", string.upper))
    local win = hs.window.focusedWindow()
    if win then
      if win:isFullScreen() then
        -- In fullscreen split mode on El capitan, the focusWindowEast (etc) functions
        -- don't work. However, we can still switch directly to named windows.
        switchFocusWithinCurrentSpace()
      else
        win[focusFn]()
      end
    end
    tmuxirl:exitUnlessStickyMode()
  end)
end)


-- Rotate between all windows in the current space.
-- TODO: reset index when moving spaces
currentSpaceWindowIndex = 0
function switchFocusWithinCurrentSpace ()
  local thisWin = hs.window.focusedWindow()
  local allWins = hs.window.filter.defaultCurrentSpace:getWindows(hs.window.filter.sortByCreated)

  local fullscreenWins = {}

  hs.fnutils.each(allWins, function(win)
    if hs.window.isFullScreen(win) and win:screen() == thisWin:screen() then
      fullscreenWins[#fullscreenWins + 1] = win
    end
  end)

  if fullscreenWins[currentSpaceWindowIndex +1] == thisWin then
    currentSpaceWindowIndex = currentSpaceWindowIndex + 1
  end
  if currentSpaceWindowIndex >= #fullscreenWins then currentSpaceWindowIndex = 0 end
  fullscreenWins[currentSpaceWindowIndex + 1]:focus()
  tmuxirl:exitUnlessStickyMode()
  currentSpaceWindowIndex = currentSpaceWindowIndex + 1
end
tmuxirl:bind('', "o", function () switchFocusWithinCurrentSpace() end)


-- swap to between the two most recent windows (from anywhere)
function switchFocusRecent ()
  local win = hs.window.filter.default:getWindows(hs.window.filter.sortByFocusedLast)[2]
  if win then
    win:focus()
  end
  tmuxirl:exitUnlessStickyMode()
end
tmuxirl:bind('ctrl', "`", function () switchFocusRecent() end)
  

-- HJKL to reposition the current window.
-- Repeat to cycle through the size variations.
hjklGrid = {
  {key="h", units={positions.left50, positions.left66, positions.left34}},
  {key="j", units={positions.bottom50, positions.bottom66, positions.bottom34}},
  {key="k", units={positions.top50, positions.top66, positions.top34}},
  {key="l", units={positions.right50, positions.right66, positions.right34}},
}
hjklGridIndexes = {}
hjklTimeoutFns = {}
hs.fnutils.each(hjklGrid, function(entry)
  hjklGridIndexes[entry.key] = {}
  hjklTimeoutFns[entry.key] = {}
  tmuxirl:bind({'shift'}, entry.key, function()
    local thisWindow = hs.window.focusedWindow()
    local thisIndex = hjklGridIndexes[entry.key][thisWindow:id()]
    local thisTimeoutFn = hjklTimeoutFns[entry.key][thisWindow:id()]

    -- Stop existing timer, start new one
    if thisTimeoutFn then thisTimeoutFn:stop() end
    thisTimeoutFn = function ()
      hjklGridIndexes[entry.key][thisWindow:id()] = 0
    end
    hs.timer.doAfter(10, thisTimeoutFn)
    hjklTimeoutFns[entry.key][thisWindow:id()] = timeoutFn

    if thisIndex == nil then thisIndex = 0 end
    if thisIndex == #entry.units then thisIndex = 0 end
    thisWindow:moveToUnit(entry.units[thisIndex + 1])
    thisIndex = thisIndex + 1
    hjklGridIndexes[entry.key][thisWindow:id()] = thisIndex
    tmuxirl:exitUnlessStickyMode()
  end)
end)


-- { } to open fullscreen splits
tmuxirl:bind('shift', '[', function ()
  local win = hs.window.focusedWindow()
  if win:isFullScreen() then
    win:toggleFullScreen()
    hs.timer.doAfter(1, function ()
      hs.eventtap.keyStroke({"ctrl", "alt", "cmd", "shift"}, "left")
    end)
  else
    hs.eventtap.keyStroke({"ctrl", "alt", "cmd", "shift"}, "left")
  end
  tmuxirl:exitUnlessStickyMode()
end)
tmuxirl:bind('shift', ']', function ()
  local win = hs.window.focusedWindow()
  if win:isFullScreen() then
    hs.timer.doAfter(1, function ()
      hs.eventtap.keyStroke({"ctrl", "alt", "cmd", "shift"}, "right")
    end)
  else
    hs.eventtap.keyStroke({"ctrl", "alt", "cmd", "shift"}, "right")
  end
  tmuxirl:exitUnlessStickyMode()
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
  tmuxirl:exitUnlessStickyMode()
end)


-- n and p to visit next/previous spaces.
-- AFAICT Hammerspoon doesn't officially support a spaces API,
-- so I'm achieving this by executing the default OS X
-- shortcuts.
tmuxirl:bind('', "n", function ()
  hs.eventtap.keyStroke({"ctrl"}, "right")
  tmuxirl:exitUnlessStickyMode()
end)
tmuxirl:bind('', "p", function ()
  hs.eventtap.keyStroke({"ctrl"}, "left")
  tmuxirl:exitUnlessStickyMode()
end)


-- For repositioning the current window to the next/previous space,
-- we have to go one step further: there don't seem to be
-- official OS X bindings for this, so these correspond
-- to bindings that I've setup in BetterTouchTool.
tmuxirl:bind('shift', "n", function ()
  hs.eventtap.keyStroke({"ctrl", "alt", "cmd"}, "right")
  tmuxirl:exitUnlessStickyMode()
end)
tmuxirl:bind('shift', "p", function ()
  hs.eventtap.keyStroke({"ctrl", "alt", "cmd"}, "left")
  tmuxirl:exitUnlessStickyMode()
end)


-- Z to toggle fullscreen...
tmuxirl:bind('shift', "z", function ()
  local win = hs.window.focusedWindow()
  win:toggleFullScreen()
  tmuxirl:exitUnlessStickyMode()
end)


--- ...and 'z' toggle maximising the window
center_toggle = {}
tmuxirl:bind('', "z", function ()
  local win = hs.window.focusedWindow()
  win:moveToUnit(positions.maximized)
  tmuxirl:exitUnlessStickyMode()
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
  tmuxirl:exitUnlessStickyMode()
end)

hs.hints.hintChars = {"j", "f", "k", "d", "l", "s", "a"}
tmuxirl:bind('', "q", function ()
  hs.hints.windowHints()
  tmuxirl:exitUnlessStickyMode()
end)


-- Launch individual application
launch_bindings = {
   {key="e", app="Emacs"},
   {key="i", app="iTerm"},
   {key="f", app="Firefox"},
   {key="s", app="Slack"},
   {key="t", app="TogglDesktop"},
}
hs.fnutils.each(launch_bindings, function(entry)
  tmuxirl:bind('', entry.key, function ()
    hs.application.launchOrFocus(entry.app)
    tmuxirl:exitUnlessStickyMode(5)
  end)
end)


-- Quick switching
tmuxirl:bind('', ';', function ()
  local win = hs.window.focusedWindow()
  if win:application():name() == "Emacs" then
     hs.application.launchOrFocus("iTerm")
  else
     hs.application.launchOrFocus("Emacs")
  end
  tmuxirl:exitUnlessStickyMode()
end)


-- Expose
tmuxirl:bind('', 'space', function ()
  hs.eventtap.keyStroke({"ctrl", "cmd", "alt"}, "space")
  tmuxirl:exitUnlessStickyMode()
end)


-- For safety, disable some keys that I dn't want to pass through.
-- I'd like it if all keys were disabled, but don't think this is a feature.
nil_chars = {"'", "[", "]", ",", "\\", "r"}
hs.fnutils.each(nil_chars, function(nil_char)
  tmuxirl:bind('', nil_char, function () return end)
end)
nil_chars_ctrl = {"z"}
hs.fnutils.each(nil_chars_ctrl, function(nil_char)
  tmuxirl:bind('ctrl', nil_char, function () return end)
end)


hs.alert("Hammerspoon!", 0.5)
logger.f('...done.')
