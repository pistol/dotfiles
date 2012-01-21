-- See https://github.com/aim-stuff/cmd-key-happy

function Set(t)
   local s = {}
   for _,v in pairs(t) do s[v] = true end
   return s
end

function set_contains(t, e)
   return t[e]
end

-- The set of global shortcuts we don't want to swap cmd/alt.

global_excludes = Set{ "shift-cmd-tab",
                       "cmd-tab",
                       "cmd-space",
                       "shift-cmd-space",
                       "cmd-,",
                       "shift-cmd-x",
                       "control-cmd-left",
                       "control-cmd-right",
                       "control-cmd-up",
                       "control-cmd-down",
                     }

-- The set of apps we want to consider swapping keys for, with some
-- notable exclusions. The exclusion means that a "cmd-w" will do the
-- normal OS Terminal behaviour. If you omit items then you would
-- have to use "alt-w" to close a terminal window.

terminal_excludes = Set{ "shift-cmd-[",
                         "shift-cmd-]",
                         "cmd-c",
                         "cmd-v",
                         "cmd-w",
                         "cmd-t",
                         "cmd-n",
                         "cmd-`",
                         "cmd-k",
                       }

apps = {
   Terminal = { exclude = terminal_excludes },
   iTerm    = { exclude = Set{
                   "cmd-c", -- copy
                   "cmd-v", -- paste
                   "cmd-`", -- switch windows
                   "shift-cmd-`", -- switch windows
                   -- "shift-cmd-[", -- switch to prev tab
                   -- "shift-cmd-]", -- switch to next tab
                   -- "cmd-w", -- close window
                   -- "cmd-t", -- new tab
                   "cmd-n", -- new window
                   -- "cmd-k", -- clear buffer
                   "cmd-]", -- next pane
                   "cmd-[", -- prev pane
                   -- "cmd-u", -- toggle transparency
                   "cmd-d", -- split vertically
                   "shift-cmd-d", -- split horizontally
                   -- "cmd-q", -- quit
                   -- "cmd-h", -- hide window
                   "cmd-return", -- fullscreen
                   "control-alt-left",  -- resize windows (emacs)
                   "control-alt-right", -- resize windows (emacs)
                   "control-alt-up",    -- resize windows (emacs)
                   "control-alt-down",  -- resize windows (emacs)
                   -- "control-meta-m",  -- fullscreen window (divvy)
                             }
   },
   -- Emacs    = { exclude = Set{ "cmd-h", -- hide window
   --                           },
   --   X11      = { exclude = { "cmd-n",  -- new terminal
   --                          } },
}

-- Return true to swap cmd/alt, otherwise false.

-- This function is passed a table comprising the following keys:
--
--   key_str_seq	key sequence (e.g., "shift-cmd-e")
--   alt		true if the alt key was pressed
--   fn                 true if the fn key was pressed
--   control            true if the control key was pressed
--   shift              true if the shift key was pressed
--   cmd                true if the command key was pressed
--   keycode		numeric virtual keycode (e.g., 48)
--   appname            the frontmost application (e.g., Terminal)
--
-- The order of the modifier keys in key-str-eq is always:
--   shift control alt cmd fn, separated by a hyphen ("-").

function swap_keys(t)
   -- for i,v in pairs(t) do print(i,v) end
   -- print(t.appname)
   if set_contains(global_excludes, t.key_str_seq) then
      return false
   end
   if not apps[t.appname] then
      return false
   end
   local excludes = apps[t.appname]["exclude"]
   if set_contains(excludes, t.key_str_seq) then
      -- print("exluding: ", t.key_str_seq)
      return false
   end
   return true
end
