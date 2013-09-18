#!/usr/bin/env ruby

DATA.each do |line|
  schema,key,*rest = line.split
  puts "gsettings set #{schema} #{key} \"#{rest.join(" ")}\""
end

__END__
org.gnome.libgnomekbd.keyboard options ['terminate\tterminate:ctrl_alt_bksp', 'caps\tcaps:ctrl_modifier']
org.gnome.desktop.wm.keybindings activate-window-menu ['<Super>space']
org.gnome.desktop.wm.keybindings maximize ['<Super>Page_Up']
org.gnome.desktop.wm.keybindings unmaximize ['<Super>Page_Down']
org.gnome.desktop.wm.keybindings move-to-workspace-down ['<Shift><Super>Down']
org.gnome.desktop.wm.keybindings move-to-workspace-left ['<Shift><Super>Left']
org.gnome.desktop.wm.keybindings move-to-workspace-right ['<Shift><Super>Right']
org.gnome.desktop.wm.keybindings move-to-workspace-up ['<Shift><Super>Up']
org.gnome.desktop.wm.keybindings switch-to-workspace-down ['<Super>Down']
org.gnome.desktop.wm.keybindings switch-to-workspace-left ['<Super>Left']
org.gnome.desktop.wm.keybindings switch-to-workspace-right ['<Super>Right']
org.gnome.desktop.wm.keybindings switch-to-workspace-up ['<Super>Up']
org.compiz.integrated show-hud ['<Super>h']
org.gnome.desktop.wm.keybindings show-desktop ['<Super>d']
org.gnome.settings-daemon.plugins.media-keys screensaver '<Shift><Super>l'
org.gnome.settings-daemon.plugins.media-keys terminal '<Super>r'
