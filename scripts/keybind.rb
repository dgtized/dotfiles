#!/usr/bin/env ruby

DATA.each do |line|
  schema,key,keybind = line.split
  puts "gsettings set #{schema} #{key} \"#{keybind}\""
end

replacements = "s/_c022d=zoomin/_c022d=pageup/;s/_c022e=zoomout/_c022e=pagedown/"
puts "sudo sed -ie '#{replacements}' /lib/udev/hwdb.d/60-keyboard.hwdb"
puts "sudo udevadm hwdb --update"

custom = "org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:" +
         "/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom%d/"
home = ENV["HOME"]
[
  ["emacs", "#{home}/usr/bin/launch_raise emacs", "<Super>j"],
  ["terminal", "#{home}/usr/bin/launch_raise Terminal", "<Super>k"],
  ["chrome", "#{home}/usr/bin/launch_raise Chrome", "<Super>h"]
].each_with_index do |commands, command_idx|
  %w{name command binding}.each_with_index do |key, offset|
    puts "gsettings set #{custom} %s '%s'" % [command_idx, key, commands[offset]]
  end
end

# fractional scaling for 125% display scale
# https://www.omgubuntu.co.uk/2019/06/enable-fractional-scaling-ubuntu-19-04
puts "gsettings set org.gnome.mutter experimental-features \"['x11-randr-fractional-scaling']\""

__END__
org.gnome.desktop.input-sources xkb-options ['caps:ctrl_modifier']
org.gnome.desktop.wm.keybindings activate-window-menu ['<Super>space']
org.gnome.desktop.wm.keybindings close ['<Super>F4']
org.gnome.desktop.wm.keybindings maximize ['<Super>Page_Up']
org.gnome.desktop.wm.keybindings unmaximize ['<Super>Page_Down']
org.gnome.desktop.wm.keybindings move-to-workspace-down ['<Shift><Super>Page_Down']
org.gnome.desktop.wm.keybindings move-to-workspace-up ['<Shift><Super>Page_Up']
org.gnome.desktop.wm.keybindings switch-to-workspace-down ['<Super>Down']
org.gnome.desktop.wm.keybindings switch-to-workspace-up ['<Super>Up']
org.gnome.desktop.wm.keybindings switch-windows ['<Alt>Tab']
org.gnome.desktop.wm.keybindings switch-applications ['<Super>Tab']
org.gnome.desktop.wm.keybindings panel-run-dialog ['<Super>F2']
org.gnome.desktop.wm.keybindings 'begin-move' ['<Super>F7']
org.gnome.desktop.wm.keybindings 'begin-resize' ['<Super>F8']
org.gnome.desktop.wm.keybindings show-desktop ['<Super>d']
org.gnome.settings-daemon.plugins.media-keys logout '<Super>Delete'
org.gnome.settings-daemon.plugins.media-keys screensaver '<Super>l'
org.gnome.settings-daemon.plugins.media-keys terminal '<Super>r'
