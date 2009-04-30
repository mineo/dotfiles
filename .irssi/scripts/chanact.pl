use Irssi 20020101.0001 ();
use strict;
use Irssi::TextUI;

use vars qw($VERSION %IRSSI);

$VERSION = "0.5.10";
%IRSSI = (
    authors     => 'BC-bd, Veli',
    contact     => 'bd@bc-bd.org, veli@piipiip.net',
    name        => 'chanact',
    description => 'Adds new powerful and customizable [Act: ...] item (chanelnames,modes,alias). Lets you give alias characters to windows so that you can select those with meta-<char>',
    license     => 'GNU GPLv2 or later',
    url         => 'https://bc-bd.org/svn/repos/irssi/chanact'
);

# Adds new powerful and customizable [Act: ...] item (chanelnames,modes,alias).
# Lets you give alias characters to windows so that you can select those with
# meta-<char>.
#
# for irssi 0.8.2 by bd@bc-bd.org
#
# inspired by chanlist.pl by 'cumol@hammerhart.de'
#
#########
# Contributors
#########
#
# veli@piipiip.net   /window_alias code
# qrczak@knm.org.pl  chanact_abbreviate_names
# qerub@home.se      Extra chanact_show_mode and chanact_chop_status
# madduck@madduck.net Better channel aliasing (case-sensitive, cross-network)
# Jan 'jast' Krueger <jast@heapsort.de>, 2004-06-22
# Ivo Timmermans <ivo@o2w.nl>	win->{hilight} patch
# 
#########
# USAGE
###
# 
# copy the script to ~/.irssi/scripts/
#
# In irssi:
#
#		/script load chanact
#		/statusbar window add -after act chanact
#
# If you want the item to appear on another position read the help
# for /statusbar.
# To remove the [Act: 1,2,3] item type:
#
#		/statusbar window remove act
#
# To see all chanact options type:
#
# 	/ set chanact_
#
# After these steps you have your new statusbar item and you can start giving
# aliases to your windows. Go to the window you want to give the alias to
# and say:
#
#		/window_alias <alias char>
#
# You can also remove the aliases with:
#
#		/window_unalias <alias char>
#
# or in aliased window:
#
#		/window_unalias
#
# To see a list of your windows use:
#
#		/window list
#
#########
# OPTIONS
#########
#
# /set chanact_chop_status <ON|OFF>
#               * ON  : shorten (Status) to S
#               * OFF : don't do it
#
# /set chanact_show_mode <ON|OFF>
#               * ON  : show channel modes
#               * OFF : don't show channel modes
#
# /set chanact_sort_by_activity <ON|OFF>
#               * ON  : sorts the list by last activity
#               * OFF : sorts the list by window number
#
# /set chanact_display <string>
#		* string : Format String for one Channel. The following $'s are expanded:
#		    $C : Channel
#		    $N : Number of the Window
#		    $M : Mode in that channel
#		    $H : Start highlightning
#		    $S : Stop highlightning
#		* example:
#		
#		      /set chanact_display $H$N:$M.$S$C
#		      
#		    will give you on #irssi.de if you have voice
#		    
#		      [3:+.#irssi.de]
#
#		    with '3:+.' highlighted and the channel name printed in regular color
#
# /set chanact_display_alias <string>
#   as 'chanact_display' but is used if the window has an alias and
#   'chanact_show_alias' is set to on.
# 
# /set chanact_show_names <ON|OFF>
#		* ON  : show the channelnames after the number/alias
#		* OFF : don't show the names
#
# /set chanact_abbreviate_names <int>
#               * 0     : don't abbreviate
#               * <int> : strip channel name prefix character and leave only
#                         that many characters of the proper name
#
# /set chanact_show_alias <ON|OFF>
#		* ON  : show the aliase instead of the refnum
#		* OFF : shot the refnum
#
# /set chanact_header <str>
# 	* <str> : Characters to be displayed at the start of the item.
# 	          Defaults to: "Act: "
#
# /set chanact_separator <str>
#		* <str> : Charater to use between the channel entries
#
# /set chanact_autorenumber <ON|OFF>
#		* ON  : Move the window automatically to first available slot
#		        starting from "chanact_renumber_start" when assigning 
#		        an alias to window. Also moves the window back to a
#		        first available slot from refnum 1 when the window
#		        loses it's alias.
#		* OFF : Don't move the windows automatically
#
# /set chanact_renumber_start <int>
# 		* <int> : Move the window to first available slot after this
# 		          num when "chanact_autorenumber" is ON.
#
# /set chanact_remove_hash <ON|OFF>
# 		* ON  : Remove &#+!= from channel names
# 		* OFF : Don't touch channel names
#
# /set chanact_remove_prefix <string>
# 		* <string> : Regular expression used to remove from the
# 		             beginning of the channel name.
# 		* example  :
# 		    To shorten a lot of debian channels:
# 		    
# 			/set chanact_remove_prefix deb(ian.(devel-)?)?
#
# /set chanact_filter <int>
# 		* 0 : show all channels
# 		* 1 : hide channels without activity
# 		* 2 : hide channels with only join/part/etc messages
# 		* 3 : hide channels with text messages
# 		* 4 : hide all channels (now why would you want to do that)
#
#########
# HINTS
#########
#
# If you have trouble with wrong colored entries your 'default.theme' might
# be too old. Try on a shell:
#
# 	$ mv ~/.irssi/default.theme /tmp/
#
# And in irssi:
#	/reload
#	/save
#
###
#################

my %show = (
	0 => "{%n ",			# NOTHING
	1 => "{sb_act_text ",		# TEXT
	2 => "{sb_act_msg ",		# MSG
	3 => "{sb_act_hilight ",	# HILIGHT
);

my ($actString,$needRemake);

sub expand {
  my ($string, %format) = @_;
  my ($exp, $repl);
  $string =~ s/\$$exp/$repl/g while (($exp, $repl) = each(%format));
  return $string;
}

# method will get called every time the statusbar item will be displayed
# but we dont need to recreate the item every time so we first
# check if something has changed and only then we recreate the string
# this might just save some cycles
# FIXME implement $get_size_only check, and user $item->{min|max-size}
sub chanact {
	my ($item, $get_size_only) = @_;

	if ($needRemake) {
		remake();
	}
	
	$item->default_handler($get_size_only, $actString, undef, 1);
}

# this is the real creation method
sub remake() {
	my ($afternumber,$finish,$hilight,$mode,$number,$display,@windows);
	my $separator = Irssi::settings_get_str('chanact_separator'); 
	my $abbrev = Irssi::settings_get_int('chanact_abbreviate_names');
	my $remove_prefix = Irssi::settings_get_str('chanact_remove_prefix');
	my $remove_hash = Irssi::settings_get_bool('chanact_remove_hash');

 	if (Irssi::settings_get_bool('chanact_sort_by_activity')) {
		@windows = sort { ($b->{last_line} <=> $a->{last_line}) }
			Irssi::windows;
	} else {
		@windows = sort { ($a->{refnum}) <=> ($b->{refnum}) }
			Irssi::windows;
	}

	$actString = "";
	foreach my $win (@windows) {
	
		# since irssi is single threaded this shouldn't happen
		!ref($win) && next;

		my $active = $win->{active};
		!ref($active) && next;

		my $name = $win->get_active_name;

		# (status) is an awfull long name, so make it short to 'S'
		# some people don't like it, so make it configurable
		if (Irssi::settings_get_bool('chanact_chop_status')
		    && $name eq "(status)") {
			$name = "S";
		}
	
		# check if we should show the mode
		$mode = "";
		if ($active->{type} eq "CHANNEL") {
			my $server = $win->{active_server};
			!ref($server) && next;

			my $channel = $server->channel_find($name);
			!ref($channel) && next;

			my $nick = $channel->nick_find($server->{nick});
			!ref($nick) && next;
			
			if ($nick->{op}) {
				$mode = "@";
			} elsif ($nick->{voice}) {
				$mode = "+";
			} elsif ($nick->{halfop}) {
				$mode = "%";
			}
		}

		next if (Irssi::settings_get_int('chanact_filter') > $win->{data_level});

		# in case we have a specific hilightcolor use it
		if ($win->{hilight_color}) {
			$hilight = "{sb_act_hilight_color $win->{hilight_color} ";
		} else {
			$hilight = $show{$win->{data_level}};
		}

		if ($remove_prefix) {
			$name =~ s/^([&#+!=]?)$remove_prefix/$1/;
		}
		if ($abbrev) {
			if ($name =~ /^[&#+!=]/) {
				$name = substr($name, 1, $abbrev + 1);
			} else {
				$name = substr($name, 0, $abbrev);
			}
		}
		if ($remove_hash) {
			$name =~ s/^[&#+!=]//;
		}

		if (Irssi::settings_get_bool('chanact_show_alias') == 1 && 
				$win->{name} =~ /^([a-zA-Z+]):(.+)$/) {
			$number = "$1";
			$display = Irssi::settings_get_str('chanact_display_alias'); 
		} else {
			$number = $win->{refnum};
			$display = Irssi::settings_get_str('chanact_display'); 
		}

		$actString .= expand($display,"C",$name,"N",$number,"M",$mode,"H",$hilight,"S","}{sb_background}").$separator;
	}

	# assemble the final string
	if ($actString ne "") {
		# Remove the last separator
		$actString =~ s/$separator$//;
		
		if (Irssi::settings_get_int('chanact_filter')) {
			$actString = "{sb ".Irssi::settings_get_str('chanact_header').$actString."}";
		} else {
			$actString = "{sb ".$actString."}";
		}
	}

	# no remake needed any longer
	$needRemake = 0;
}

# method called because of some events. here we dont remake the item but just
# remember that we have to remake it the next time we are called
sub chanactHasChanged()
{
	# if needRemake is already set, no need to trigger a redraw as we will
	# be redrawing the item anyway.
	return if $needRemake;

	$needRemake = 1;

	Irssi::statusbar_items_redraw('chanact');
}

# function by veli@piipiip.net
# Remove alias
sub cmd_window_unalias {
	my ($data, $server, $witem) = @_;
	my $rn_start = Irssi::settings_get_int('chanact_renumber_start');
	
	unless ($data =~ /^[a-zA-Z]$/ || 
			Irssi::active_win()->{name} =~ /^[a-zA-Z]$/) {
		Irssi::print("Usage: /window_unalias <char>");
		Irssi::print("or /window_alias in window that has an alias.");
		return;
	}
	
	if ($data eq '') { $data = Irssi::active_win()->{name}; }

	if (my $oldwin = Irssi::window_find_name($data)) {
		$oldwin->set_name(undef);
		Irssi::print("Removed alias with the key '$data'.");
		
		if (Irssi::settings_get_bool('chanact_autorenumber') == 1 && 
				$oldwin->{refnum} >= $rn_start) {
			my $old_refnum = $oldwin->{refnum};
			
			# Find the first available slot and move the window
			my $newnum = 1;
			while (Irssi::window_find_refnum($newnum) ne "") { $newnum++; }
			$oldwin->set_refnum($newnum);
			
			Irssi::print("and moved it to from $old_refnum to $newnum");
		}
	}
}

# function by veli@piipiip.net
# Make an alias
sub cmd_window_alias {
	my ($data, $server, $witem) = @_;
	my $rn_start = Irssi::settings_get_int('chanact_renumber_start');

	unless ($data =~ /^[a-zA-Z+]$/) {
		Irssi::print("Usage: /window_alias <char>");
		return;
	}

	cmd_window_unalias($data, $server, $witem);
	
	my $window = $witem->window();
	my $winnum = $window->{refnum};
	
	if (Irssi::settings_get_bool('chanact_autorenumber') == 1 &&
			$window->{refnum} < $rn_start) {
		my $old_refnum = $window->{refnum};

		$winnum = $rn_start;
 
		# Find the first available slot and move the window
		while (Irssi::window_find_refnum($winnum) ne "") { $winnum++; }
		$window->set_refnum($winnum);
		
		Irssi::print("Moved the window from $old_refnum to $winnum");
	}
	
	my $winname = $witem->{name};
	my $winserver = $window->{active_server}->{tag};
	my $winhandle = "$winserver/$winname";
	$window->set_name("$data:$winhandle");
	$server->command("/bind meta-$data change_window $data:$winhandle");
	Irssi::print("Window $winhandle is now accessible with meta-$data");
}

# function by veli@piipiip.net
# Makes the aliases if names have already been set
sub cmd_rebuild_aliases {
	foreach (sort { $a->{refnum} <=> $b->{refnum} } Irssi::windows) {
		if ($_->{name} =~ /^[a-zA-Z]$/) {
			cmd_window_alias($_->{name}, $_->{active_server}, $_->{active});
		}
	}
}

# function by veli@piipiip.net
# Change the binding if the window refnum changes.
sub refnum_changed {
	my ($window, $oldref) = @_;
	my $server = Irssi::active_server();

	if ($window->{name} =~ /^[a-zA-Z]$/) {
		$server->command("/bind meta-".$window->{name}." change_window ".$window->{refnum});
	}
}

$needRemake = 1;

# Window alias command
Irssi::command_bind('window_alias','cmd_window_alias');
Irssi::command_bind('window_unalias','cmd_window_unalias');
# Irssi::command_bind('window_alias_rebuild','cmd_rebuild_aliases');

# our config item
Irssi::settings_add_str('chanact', 'chanact_display', '$H$N:$M$C$S');
Irssi::settings_add_str('chanact', 'chanact_display_alias', '$H$N$M$S');
Irssi::settings_add_int('chanact', 'chanact_abbreviate_names', 0);
Irssi::settings_add_bool('chanact', 'chanact_show_alias', 1);
Irssi::settings_add_str('chanact', 'chanact_separator', " ");
Irssi::settings_add_bool('chanact', 'chanact_autorenumber', 0);
Irssi::settings_add_bool('chanact', 'chanact_remove_hash', 0);
Irssi::settings_add_str('chanact', 'chanact_remove_prefix', "");
Irssi::settings_add_int('chanact', 'chanact_renumber_start', 50);
Irssi::settings_add_str('chanact', 'chanact_header', "Act: ");
Irssi::settings_add_bool('chanact', 'chanact_chop_status', 1);
Irssi::settings_add_bool('chanact', 'chanact_sort_by_activity', 1);
Irssi::settings_add_int('chanact', 'chanact_filter', 0);

# register the statusbar item
Irssi::statusbar_item_register('chanact', '$0', 'chanact');
# according to cras we shall not call this
# Irssi::statusbars_recreate_items();

# register all that nifty callbacks on special events
Irssi::signal_add_last('setup changed', 'chanactHasChanged');
Irssi::signal_add_last('window changed', 'chanactHasChanged');
Irssi::signal_add_last('window item changed', 'chanactHasChanged');
Irssi::signal_add_last('window hilight', 'chanactHasChanged');
Irssi::signal_add_last('window item hilight', 'chanactHasChanged');
Irssi::signal_add("window created", "chanactHasChanged");
Irssi::signal_add("window destroyed", "chanactHasChanged");
Irssi::signal_add("window name changed", "chanactHasChanged");
Irssi::signal_add("window activity", "chanactHasChanged");
Irssi::signal_add("print text", "chanactHasChanged");
Irssi::signal_add('nick mode changed', 'chanactHasChanged');

Irssi::signal_add_last('window refnum changed', 'refnum_changed');

###############
###
#
# Changelog
# 
# 0.5.10
# 	- fixed irssi crash when using Irssi::print from within remake()
#       - added option to filter out some data levels, based on a patch by
#         Juergen Jung <juergen@Winterkaelte.de>, see
#         https://bc-bd.org/trac/irssi/ticket/15
#         	+ retired chanact_show_all in favour of chanact_filter
#
# 0.5.9
# 	- changes by stefan voelkel
# 		+ sort channels by activity, see
# 		  https://bc-bd.org/trac/irssi/ticket/5, based on a patch by jan
# 		  krueger
# 		+ fixed chrash on /exec -interactive, see
# 		https://bc-bd.org/trac/irssi/ticket/7
#
# 	- changes by Jan 'jast' Krueger <jast@heapsort.de>, 2004-06-22
# 		+ updated documentation in script's comments
#
# 	- changes by Ivo Timmermans <ivo@o2w.nl>
# 		+ honor actcolor /hilight setting if present
#
# 0.5.8
# - made aliases case-sensitive and include network in channel names by madduck
#
# 0.5.7
# - integrated remove patch by Christoph Berg <myon@debian.org>
#
# 0.5.6
# - fixed a bug (#1) reported by Wouter Coekaert
# 
# 0.5.5
# - some speedups from David Leadbeater <dgl@dgl.cx>
# 
#
# 0.5.4
# - added help for chanact_display_alias
#
# 0.5.3
# - added '+' to the available chars of aliase's
# - added chanact_display_alias to allow different display modes if the window
#   has an alias
#
# 0.5.2
# - removed unused chanact_show_name settings (thx to Qerub)
# - fixed $mode display
# - guarded reference operations to (hopefully) fix errors on server disconnect
# 
# 0.5.1
# - small typo fixed
#
# 0.5.0
# - changed chanact_show_mode to chanact_display. reversed changes from
#   Qerub through that, but kept funcionality.
# - removed chanact_color_all since it is no longer needed
# 
# 0.4.3
# - changes by Qerub
#   + added chanact_show_mode to show the mode just before the channel name
#   + added chanact_chop_status to be able to control the (status) chopping
#     [bd] minor implementation changes
# - moved Changelog to the end of the file since it is getting pretty big
#
# 0.4.2
# - changed back to old version numbering sheme
# - added '=' to Qrczak's chanact_abbreviate_names stuff :)
# - added chanact_header
#
# 0.41q
#	- changes by Qrczak
#		+ added setting 'chanact_abbreviate_names'
#		+ windows are sorted by refnum; I didn't understand the old
#		  logic and it broke sorting for numbers above 9
#
# 0.41
#	- minor updates
#		+ fixed channel sort [veli]
#		+ removed few typos and added some documentation [veli]
#
# 0.4
#	- merge with window_alias.pl
#		+ added /window_alias from window_alias.pl by veli@piipiip.net
#		+ added setting 'chanact_show_alias'
#		+ added setting 'chanact_show_names'
#		+ changed setting 'chanact_show_mode' to int
#		+ added setting 'chanact_separator' [veli]
#		+ added setting 'chanact_autorenumber' [veli]
#		+ added setting 'chanact_renumber_start' [veli]
#		+ added /window_unalias [veli]
#		+ moved setting to their own group 'chanact' [veli]
#
# 0.3
#	- merge with chanlist.pl
#		+ added setting 'chanact_show_mode'
#		+ added setting 'chanact_show_all'
#
# 0.2
#	- added 'Act' to the item
#		- added setting 'chanact_color_all'
#		- finally found format for statusbar hilight
#
# 0.1
#	- Initial Release
#
###
################
