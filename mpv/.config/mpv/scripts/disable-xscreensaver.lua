local utils = require 'mp.utils'
local msg = require 'mp.msg'

timer = mp.add_periodic_timer(2, function()
                        msg.verbose("Calling xscreensaver-command -deactivate")
                        proc_table = {["args"] = {"xscreensaver-command", "-demo"}}
                        proc = utils.subprocess(proc_table)
                        if proc.error ~= nil then
                          msg.info("Couldn't call xscreensaver")
                          timer:kill()
                        end
end)
