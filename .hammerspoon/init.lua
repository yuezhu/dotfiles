log = hs.logger.new('init.lua', 'debug')

cfg = {}

-- hyper bound to cmd+ctrl
cfg.hyper = {'cmd', 'ctrl'}

cfg.alert = {}
cfg.alert.style = {}
cfg.alert.style.default = {
  fillColor       = {white = 0, alpha = 0.5},
  strokeColor     = {alpha = 0},
  radius          = 5,
  textSize        = 28,
  textFont        = hs.styledtext.defaultFonts.boldSystem.name
}

-- This also requires alert.show function call to have the last argument 0 to
-- disable fade in/out.
cfg.alert.style.noFadeInOut = {
  fadeInDuration  = 0,
  fadeOutDuration = 0,
  fillColor       = {white = 0, alpha = 0.5},
  strokeColor     = {alpha = 0},
  radius          = 5,
  textSize        = 28,
  textFont        = hs.styledtext.defaultFonts.boldSystem.name
}

-- Animation when resizing windows
hs.window.animationDuration = 0

--------------------------------------------------------------------------------
-- Window layout management (focused window only)
--------------------------------------------------------------------------------
cfg.window = {}

-- From https://github.com/ahonn/dotfiles/blob/master/hammerspoon/modules/window.lua
cfg.window.createResizer = function(winObj)
  -- winObj:screen():fullFrame() return 0.0, 0.0, 1920.0, 1080.0
  -- winObj:screen():frame() return 0.0, 23.0, 1920.0, 1057.0
  local screenFrame = winObj:screen():frame()
  local windowFrame = winObj:frame()
  local zoomStep = {
    w = screenFrame.w / 30,
    h = screenFrame.h / 30
  }

  return {
    left = function(scale)
      winObj:setFrame(hs.geometry.rect(
          screenFrame.x,
          screenFrame.y,
          screenFrame.w * scale,
          screenFrame.h))
    end,

    right = function(scale)
      winObj:setFrame(hs.geometry.rect(
          screenFrame.x + screenFrame.w * scale,
          screenFrame.y,
          screenFrame.w * scale,
          screenFrame.h))
    end,

    top = function(scale)
      winObj:setFrame(hs.geometry.rect(
          screenFrame.x,
          screenFrame.y,
          screenFrame.w,
          screenFrame.h * scale))
    end,

    bottom = function(scale)
      winObj:setFrame(hs.geometry.rect(
          screenFrame.x,
          screenFrame.y + screenFrame.h * scale,
          screenFrame.w,
          screenFrame.h * scale))
    end,

    center = function()
      winObj:centerOnScreen()
    end,

    resize = function(size)
      winObj:setFrame(hs.geometry.rect(math.max(windowFrame.x - (zoomStep.w * size), 0),
          math.max(windowFrame.y - (zoomStep.h * size), 0),
          math.min(windowFrame.w + (zoomStep.w * 2 * size), screenFrame.w),
          math.min(windowFrame.h + (zoomStep.h * 2 * size), screenFrame.h)))
    end,
  }
end

cfg.window.resize = function(option)
  local winObj = hs.window.focusedWindow()
  if winObj ~= nil and not winObj:isFullScreen() then
    local resizer = cfg.window.createResizer(winObj)
    if option == 'Left' then
      resizer.left(1/2)
    elseif option == 'Right' then
      resizer.right(1/2)
    elseif option == 'Top' then
      resizer.top(1/2)
    elseif option == 'Bottom' then
      resizer.bottom(1/2)
    elseif option == 'Center' then
      resizer.center()
    elseif option == 'Enlarge' then
      resizer.resize(1)
    elseif option == 'Shrink' then
      resizer.resize(-1)
    end
  end
end

-- cfg.window.cache holds maximized window ID. It memorizes maximized windows
-- so that they can be restored to the original size.
cfg.window.cache = {}

-- Destroyed window ID must be cleared from the cache
cfg.window.filter = hs.window.filter.new(true)
cfg.window.filter:subscribe(
  hs.window.filter.windowDestroyed,
  function(winObj)
    local winId = winObj:id()
    if cfg.window.cache[winId] ~= nil then
      log.df('remove window ID %s from cache', winId)
      cfg.window.cache[winId] = nil
    end
end)

cfg.window.toggleMaximize = function()
  local winObj = hs.window.focusedWindow()
  local winId = winObj:id()
  local winFrame = winObj:frame()
  local scrFrame = winObj:screen():frame()

  if (winObj == nil or
    not winObj:isMaximizable() or
    winObj:subrole() ~= 'AXStandardWindow' or
    winObj:isFullScreen()) then
    return
  end

  local cached = cfg.window.cache[winId]

  if cached == nil then
    cfg.window.cache[winId] = winFrame
    winObj:maximize()
    log.df('maximize window ID %s', winId)
  else
    -- add a slight offset to compensate the negative x or y.
    local sameScreen = hs.geometry.point(cached.x+10, cached.y+10):inside(scrFrame)
    if sameScreen then
      cfg.window.cache[winId] = nil
      winObj:setFrame(cached)
      log.df('restore window ID %s', winId)
    else
      cfg.window.cache[winId] = winFrame
      winObj:maximize()
      log.df('maximize window ID %s (different screen)', winId)
    end
  end
end

cfg.window.toggleFullScreen = function ()
  local winObj = hs.window.focusedWindow()
  if winObj ~= nil then
    winObj:toggleFullScreen()
  end
end

--------------------------------------------------------------------------------
-- Audio
--------------------------------------------------------------------------------
cfg.audio = {}

cfg.audio.default = {
  ['input'] = {
    'Yue’s AirPods',
    'External Microphone',
    'MacBook Pro Microphone',
  },
  ['output'] = {
    'Yue’s AirPods',
    'External Headphones',
    'MacBook Pro Speakers',
  },
}

--------------------------------------------------------------------------------
-- Input methods auto switcher
--------------------------------------------------------------------------------
cfg.inputMethod = {}

cfg.inputMethod.rules = {
  ['1Password 7']      = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Activity Monitor'] = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['AliWangwang']      = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.inputmethod.SCIM.ITABC', _, _, _)},
  ['App Store']        = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Audirvana']        = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Authy Desktop']    = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Dictionary']       = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Emacs']            = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Evernote']         = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['FaceTime']         = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Finder']           = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Google Chrome']    = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Hammerspoon']      = {hs.window.filter.windowCreated, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Music']            = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Safari']           = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Skype']            = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.inputmethod.SCIM.ITABC', _, _, _)},
  ['Slack']            = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Spotlight']        = {hs.window.filter.windowCreated, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Sublime Text']     = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['Terminal']         = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US')},
  ['WeChat']           = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.inputmethod.SCIM.ITABC', _, _, _)},
  ['iTerm2']           = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
  ['iTunes']           = {hs.window.filter.windowFocused, hs.fnutils.partial(hs.keycodes.currentSourceID, 'com.apple.keylayout.US', _, _, _)},
}

cfg.inputMethod.filters = {}

for app, evt in pairs(cfg.inputMethod.rules) do
  cfg.inputMethod.filters[app] = hs.window.filter.new(app)
  cfg.inputMethod.filters[app]:subscribe(evt[1], evt[2])
end

--------------------------------------------------------------------------------
-- Turn WiFi on/off depending on the wired network's link state.
--------------------------------------------------------------------------------
cfg.network = {}

-- cfg.network.findInterface returns a table holding information about
-- specified network interface, or nil if not found. The input parameter name
-- is user defined name and is identical to what is shown in System
-- Preferences -> Network. For example, for input "Thunderbolt Ethernet Slot
-- 1, Port 1", the return, eg. may look like:
-- {
--   DeviceName = "en5",
--   Hardware = "Ethernet",
--   Type = "Ethernet",
--   UserDefinedName = "Thunderbolt Ethernet Slot  1, Port 1"
-- }
cfg.network.findInterface = function(name)
  local net = hs.network.configuration.open()
  for k, v in pairs(net:contents('Setup:/Network/Service/.+/Interface', true)) do
    if v.UserDefinedName ~= nil and string.find(v.UserDefinedName, name) ~= nil then
      return v
    end
  end
  return nil
end

-- cfg.network.setWiFiPower sets WiFi power state and informs it using
-- on-screen alerts.
cfg.network.setWiFiPower = function(state)
  hs.alert.closeSpecific(cfg.network.alert)
  local msg = string.format('     Turn Wi-Fi %s   ', state and 'On' or 'Off')
  cfg.network.alert = hs.alert.show(msg, cfg.alert.style.default)
  hs.wifi.setPower(state)
end

cfg.network.wired = cfg.network.findInterface('^Thunderbolt Ethernet Slot.*')

if cfg.network.wired ~= nil then

  -- The timer is used to avoid toggling WiFi power state too frequently.
  -- In case of network blip, change WiFi power state when the interval
  -- between two consecutive events is longer than 5 seconds, or the former
  -- event will be simply discarded.
  cfg.network.timer = hs.timer.delayed.new(5,
    function ()
      if cfg.network.wiredUp then
        if hs.wifi.interfaceDetails().power then
          cfg.network.setWiFiPower(false)
        end
      else
        if not hs.wifi.interfaceDetails().power then
          cfg.network.setWiFiPower(true)
        end
      end
  end)

  cfg.network.onLinkStateChange = function ()
    local net = hs.network.configuration.open()
    local key = string.format('State:/Network/Interface/%s/Link', cfg.network.wired.DeviceName)
    local up = false

    for k, n in pairs(net:contents(key)) do
      if k == key then
        up = n.Active
        break
      end
    end

    log.df('wired network %q, link %s', cfg.network.wired.UserDefinedName, (up and 'up' or 'down'))

    -- System may wake for network access during system sleep and network
    -- interface link state may change. In this case, do not change WiFi power
    -- state.
    if cfg.systemSlept then
      return
    end

    cfg.network.wiredUp = up
    -- 11/15/21 disabled to always enable WiFi
    -- cfg.network.timer:start()
  end

  -- Start monitoring wired link state change
  cfg.network.cfg = hs.network.configuration.open()
  cfg.network.cfg:setCallback(cfg.network.onLinkStateChange)
  cfg.network.cfg:monitorKeys(string.format('State:/Network/Interface/%s/Link', cfg.network.wired.DeviceName), true)
  cfg.network.cfg:start()

  -- Make sure WiFi power state is set when hammerspoon is loaded.
  cfg.network.onLinkStateChange()

elseif not hs.wifi.interfaceDetails().power then
  cfg.network.setWiFiPower(true)
end

--------------------------------------------------------------------------------
-- Keep screen/system awake
-- Menubar icons from
-- https://github.com/Hammerspoon/Spoons/blob/master/Spoons/Caffeine.spoon.zip
--------------------------------------------------------------------------------
cfg.caffeinate = {}

-- cfg.caffeinate.screenOn controls if screen should stay on when caffeinate
-- is enabled. When false, only system sleep is prevented and screen still cal
-- be off.
cfg.caffeinate.screenOn = false

-- cfg.caffeinate.toggled tracks what duration of caffeinate is enabled.
cfg.caffeinate.toggled = {}

-- cfg.caffeinate.inform informs system awake state using on-screen alerts and
-- the menubar icon.
cfg.caffeinate.inform = function(state)
  hs.alert.closeSpecific(cfg.caffeinate.alert)
  if state then
    cfg.caffeinate.alert = hs.alert.show('     Caffeinate Activated   ', cfg.alert.style.default)
    cfg.caffeinate.menuBarItem:setIcon(hs.configdir .. '/caffeine-on.pdf')
  else
    cfg.caffeinate.alert = hs.alert.show('     Caffeinate Deactivated   ', cfg.alert.style.default)
    cfg.caffeinate.menuBarItem:setIcon(hs.configdir .. '/caffeine-off.pdf')
  end
end

-- cfg.caffeinate.stopTimer stops and destroys caffeinate timer.
cfg.caffeinate.stopTimer = function()
  if cfg.caffeinate.timer ~= nil and cfg.caffeinate.timer:running() then
    cfg.caffeinate.timer:stop()
    cfg.caffeinate.timer = nil
  end
end

-- cfg.caffeinate.activate activates caffeinate.
cfg.caffeinate.activate = function()
  hs.caffeinate.set('systemIdle', not cfg.caffeinate.screenOn)
  hs.caffeinate.set('displayIdle', cfg.caffeinate.screenOn)
end

-- cfg.caffeinate.deactivate deactivates caffeinate.
cfg.caffeinate.deactivate = function()
  hs.caffeinate.set('systemIdle', false)
  hs.caffeinate.set('displayIdle', false)
end

-- cfg.caffeinate.isActivated returns if caffeinate is activated.
cfg.caffeinate.isActivated = function()
  return hs.caffeinate.get('systemIdle') or hs.caffeinate.get('displayIdle')
end

-- cfg.caffeinate.toggle toggles system awake state with specified duration in
-- seconds - 0 for indefinitely and a negative value disables system awake. If
-- system awake is toggled when it has been enabled, the latter duration will
-- overwrite the former duration, and the duration will start again.
cfg.caffeinate.toggle = function(duration)
  cfg.caffeinate.toggled = {}
  if duration < 0 then
    if cfg.caffeinate.isActivated() then
      cfg.caffeinate.inform(false)
      cfg.caffeinate.deactivate()
    end
    cfg.caffeinate.stopTimer()
  else
    cfg.caffeinate.toggled[duration] = true
    cfg.caffeinate.inform(true)
    cfg.caffeinate.activate()

    -- Timer will deactivate caffeinate.
    if duration == 0 then
      cfg.caffeinate.stopTimer()
    else
      cfg.caffeinate.timer = hs.timer.doAfter(duration, function()
          cfg.caffeinate.toggled = {}
          cfg.caffeinate.inform(false)
          cfg.caffeinate.deactivate()
      end)
    end
  end
end

-- cfg.caffeinate.toggleScreenOn toggles if both screen and system should stay
-- awake when caffeinate is turned on.
cfg.caffeinate.toggleScreenOn = function()
  cfg.caffeinate.screenOn = not cfg.caffeinate.screenOn
  if not cfg.caffeinate.isActivated() then
    return
  end
  cfg.caffeinate.activate()
end

cfg.caffeinate.menuBarItem = hs.menubar.new()
cfg.caffeinate.menuBarItem:setIcon(hs.configdir .. '/caffeine-off.pdf')
cfg.caffeinate.menuBarItem:setMenu(function()
    return {
      {
        title = 'Keep Awake', menu = {
          {
            title   = '1 Hour',
            fn      = hs.fnutils.partial(cfg.caffeinate.toggle, hs.timer.hours(1)),
            checked = cfg.caffeinate.toggled[hs.timer.hours(1)]
          },
          {
            title   = '2 Hours',
            fn      = hs.fnutils.partial(cfg.caffeinate.toggle, hs.timer.hours(2)),
            checked = cfg.caffeinate.toggled[hs.timer.hours(2)]
          },
          {
            title   = '4 Hours',
            fn      = hs.fnutils.partial(cfg.caffeinate.toggle, hs.timer.hours(4)),
            checked = cfg.caffeinate.toggled[hs.timer.hours(4)]
          },
          {
            title   = '8 Hours',
            fn      = hs.fnutils.partial(cfg.caffeinate.toggle, hs.timer.hours(8)),
            checked = cfg.caffeinate.toggled[hs.timer.hours(8)]
          },
          {
            title   = 'Indefinitely',
            fn      = hs.fnutils.partial(cfg.caffeinate.toggle, 0),
            checked = cfg.caffeinate.toggled[0]
          },
        },
      },
      {
        title   = 'Screen On',
        fn      = cfg.caffeinate.toggleScreenOn,
        checked = cfg.caffeinate.screenOn
      },
      { title = '-' },
      {
        title = 'Disable', fn = hs.fnutils.partial(cfg.caffeinate.toggle, -1),
      },
    }
end)

--------------------------------------------------------------------------------
-- Display
--------------------------------------------------------------------------------
cfg.display = {}

cfg.display.kickTimer = hs.timer.delayed.new(5, function()
  if os.execute('/usr/bin/sudo /usr/bin/pkill -9 corebrightnessd') == true then
    log.df('kicked corebrightnessd')
  else
    log.ef('error kicking corebrightnessd')
  end
end)

--------------------------------------------------------------------------------
-- Watchers
--------------------------------------------------------------------------------
cfg.watchers = {}

cfg.watchers.systemSleep = hs.caffeinate.watcher.new(function(evt)
    if evt == hs.caffeinate.watcher.systemWillSleep then
      log.df('system is preparing to sleep')
      cfg.systemSlept = true
    elseif evt == hs.caffeinate.watcher.systemDidWake then
      log.df('system woke')
      cfg.systemSlept = false
    elseif evt == hs.caffeinate.watcher.screensDidWake then
      log.df('screens woke')

      -- kick corebrightnessd to fix TrueTone
      if hs.host.localizedName() == 'C02CC1CDMD6M' then
        cfg.display.kickTimer:start()
      end
    end
end)

cfg.watchers.systemSleep:start()

-- cfg.watchers.application = hs.application.watcher.new(function(app, evt, obj)
--     if evt == hs.application.watcher.activated then
--       if app == 'Finder' then
--         obj:selectMenuItem({'Window', 'Bring All to Front'})
--       end
--     end
-- end)

-- cfg.watchers.application:start()

-- cfg.watchers.usb = hs.usb.watcher.new(function(data)
--     -- Auto launch or quit "Epson Scan 2"
--     if data['productName'] == 'EPSON Scanner' then
--       if data['eventType'] == 'added' then
--         -- if hs.application.launchOrFocus('Epson Scan 2') then
--         --   log.df('"Epson Scan 2" launched')
--         -- end
--       elseif data['eventType'] == 'removed' then
--         local app = hs.application.get('Epson Scan 2')
--         if app ~= nil and app:activate() then
--           hs.eventtap.event.newKeyEvent(hs.keycodes.map['return'], true):post()
--           log.df('"Epson Scan 2" terminated')
--         end
--       end
--     end
-- end)

-- cfg.watchers.usb:start()

-- cfg.watchers.reload = {}

-- cfg.watchers.reload.monitored = {
--   [hs.configdir .. '/init.lua'] = true,
-- }

-- cfg.watchers.reload.timer = hs.timer.delayed.new(2, function ()
--   hs.reload()
-- end)

-- cfg.watchers.reload.watcher = hs.pathwatcher.new(
--   hs.configdir,
--   function(paths)
--     for _, p in pairs(paths) do
--       if cfg.watchers.reload.monitored[p] then
--         cfg.watchers.reload.timer:start()
--         break
--       end
--     end
--   end
-- )

-- cfg.watchers.reload.watcher:start()

--------------------------------------------------------------------------------
-- Event taps
--------------------------------------------------------------------------------
cfg.eventTaps = {}

-- Reverse scroll direction for line based scroll, which typically indicates
-- that the scroll event is triggered from an external mouse with a scroll
-- wheel.
cfg.eventTaps.scrollWheel = hs.eventtap.new({ hs.eventtap.event.types.scrollWheel },
  function(evt)
    -- The scrolling data is line-based.
    if evt:getProperty(hs.eventtap.event.properties.scrollWheelEventIsContinuous) == 0 then
      local axis1 = hs.eventtap.event.properties.scrollWheelEventDeltaAxis1
      local axis2 = hs.eventtap.event.properties.scrollWheelEventDeltaAxis2
      evt:setProperty(axis1, -evt:getProperty(axis1))
      evt:setProperty(axis2, -evt:getProperty(axis2))
    end
end)

cfg.eventTaps.scrollWheel:start()

-- Toggle window maximization state when double-click on titlebar.
cfg.eventTaps.leftMouseDoubleClicked = hs.eventtap.new({ hs.eventtap.event.types.leftMouseUp },
  function(evt)
    if evt:getProperty(hs.eventtap.event.properties.mouseEventClickState) == 2 then
      local winObj = hs.window.focusedWindow()
      if winObj ~= nil then
        local frame = winObj:frame()
        -- There is no API to get the height of window titlebar, so 30 is just
        -- an approximate value, which is good for most of the windows.
        local titlebar = hs.geometry.rect(frame.x, frame.y, frame.w, 30)
        local pos = evt:location()
        local cursor = hs.geometry.point(pos.x, pos.y)
        local toggle = false

        local excludes = {
          ['Safari'] = true,
          -- ['iTerm2'] = true
        }

        if excludes[winObj:application():title()] ~= nil then
          return
        end

        if cursor:inside(titlebar) then
          cfg.window.toggleMaximize()
        end
      end
    end
end)

-- 05/19/22 disable
-- cfg.eventTaps.leftMouseDoubleClicked:start()

--------------------------------------------------------------------------------
-- Show notification after config is reloaded
--------------------------------------------------------------------------------
hs.task.new('/bin/ps',
  function(status, stdout, stderr)
    if status ~= 0 then
      log.ef('error getting hammerspoon process etime, status %d, error %s', status, stderr)
      return
    end

    local s = {}
    for v in string.gmatch(stdout, '%d+') do
      table.insert(s, 1, v)
    end

    local etime = 0 -- hammerspoon process execution time in seconds
    for i, v in pairs(s) do
      etime = etime + v * (60 ^ (i - 1))
    end

    log.df('hammerspoon process seconds %s', etime)

    -- If hammerspoon process execution time has been there long enough, we
    -- consider this is a reload and show a notification.
    if etime > 120 then
      hs.notify.new(
        {
          title = 'Hammerspoon',
          informativeText = 'Config Reloaded',
          withdrawAfter = 3,
        }
      ):send()
    end
  end,
  {'-p', tostring(hs.processInfo.processID), '-o', 'etime='}
):start()

--------------------------------------------------------------------------------
-- Key bindings
--------------------------------------------------------------------------------
cfg.hotkey = {}

-- hs.hotkey.bind(cfg.hyper, 'r', hs.reload)
hs.hotkey.bind(cfg.hyper, 'left',  hs.fnutils.partial(cfg.window.resize, 'Left'))
hs.hotkey.bind(cfg.hyper, 'down',  hs.fnutils.partial(cfg.window.resize, 'Bottom'))
hs.hotkey.bind(cfg.hyper, 'up',    hs.fnutils.partial(cfg.window.resize, 'Top'))
hs.hotkey.bind(cfg.hyper, 'right', hs.fnutils.partial(cfg.window.resize, 'Right'))
hs.hotkey.bind(cfg.hyper, 'c',     hs.fnutils.partial(cfg.window.resize, 'Center'))
hs.hotkey.bind(cfg.hyper, '-',     hs.fnutils.partial(cfg.window.resize, 'Shrink'))
hs.hotkey.bind(cfg.hyper, '=',     hs.fnutils.partial(cfg.window.resize, 'Enlarge'))
hs.hotkey.bind(cfg.hyper, 'm',     cfg.window.toggleMaximize)
hs.hotkey.bind(cfg.hyper, 'f',     cfg.window.toggleFullScreen)
hs.hotkey.bind(cfg.hyper, 'l',     hs.caffeinate.lockScreen)

hs.hotkey.bind(cfg.hyper, 'g', function()
    -- local file = '~/bin/google-meet-safari.applescript'
    -- if hs.urlevent.getDefaultHandler('http') == 'com.google.Chrome' then
    --   file = '~/bin/google-meet-chrome.applescript'
    -- end

    -- Set audio devices based on preferences
    for _, name in ipairs(cfg.audio.default.input) do
      local device = hs.audiodevice.findInputByName(name)
      if device ~= nil then
        if device:setDefaultInputDevice() then
          log.df('set default input: %s', device:name())
          break
        end
      end
    end

    for _, name in ipairs(cfg.audio.default.output) do
      local device = hs.audiodevice.findOutputByName(name)
      if device ~= nil then
        if device:setDefaultOutputDevice() then
          log.df('set default output: %s', device:name())
          break
        end
      end
    end

    hs.osascript.applescriptFromFile(hs.fs.pathToAbsolute('~/bin/google-meet-chrome.applescript'))
end)

hs.hotkey.bind(cfg.hyper, 's', function()
    -- hold keys for a few seconds to trigger system sleep
    local delay = 2

    cfg.hotkey.timer = hs.timer.doAfter(delay, function()
        log.df('system sleep triggered')
        cfg.caffeinate.toggle(-1)
        hs.caffeinate.systemSleep()
    end)

    hs.timer.doWhile(
      function()
        return cfg.hotkey.timer:running()
      end,
      function()
        -- no fade in/out to make the alert box visually appears the same.
        hs.alert.closeSpecific(cfg.hotkey.alert, 0)
        cfg.hotkey.alert = hs.alert.show(string.format('     System Sleep In %ss   ', delay),
          cfg.alert.style.noFadeInOut,
          hs.screen.mainScreen(),
          1)
        delay = delay - 1
      end,
      1
    ):fire()
  end,

  function()
    cfg.hotkey.timer:stop()
    hs.alert.closeSpecific(cfg.hotkey.alert)
end)
