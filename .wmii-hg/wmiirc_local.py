from pygmi import *
import mpdclient2


wmii['font'] = 'xft:Lucida Grande:size=7'
wmii['normcolors'] = '#190404', '#f0f0f0', '#f0f0f0'
wmii['focuscolors'] = '#ffffff', '#2d2d2d', '#f0f0f0'

terminal = 'wmiir', 'setsid', 'urxvtc'
browser = 'wmiir', 'setsid', 'firefox'

wmii.tagrules = (
		('Thunderbird|Zim|Liferea','3'),
		('Shiretoko|Midori|Arora','1'),
		('GMPC|Picard','2'),
		('Pidgin|Skype','4'),
		)

monitors['load'].active = False
monitors['time'].active = False

mpd_connected = True
@defmonitor(interval=20,colors=wmii['focuscolors'],side='left')
def mpd2(self):
	return call('mpc').split('\n')[0]
	# if not mpd_connected:
	# 	mpdc = mpdclient2.connect()
	# 	return 'Reconnected to MPD'
# 
	# try:
	# 	if mpdc.status()['state'] == 'play':
	# 		return 'MPD '+mpdc.currentsong()['title']+' by '+mpdc.currentsong['artist']
	# 	else:
	# 		return 'MPD is not playing'
	# except:
	# 	mpd_connected = False
	# 	return 'problems with MPD'

@defmonitor(colors=wmii['focuscolors'])
def cpu(self):
	return open('/proc/cpuinfo').read().split()[29].replace('.000','')+'MHz'

@defmonitor(colors=wmii['focuscolors'])
def bat(self):
	return call('acpi').split(',')[1]

keys.bind('main', (
	('XF86HomePage', "Launch the browser",
		lambda k: call(*browser, background=True)),
	# ('%(mod)s-e',"Launch the filemanager",
		# lambda k: call(*('wmiir','setsid','thunar'),background=True),
	('XF86Favorites',"GMPC",
		lambda k: call(*('wmiir','setsid','gmpc'),background=True)),
	('XF86Mail',"KMail",
		lambda k: call(*('wmiir','setsid','kmail'),background=True)),
	('XF86Search',"Dolphin",
		lambda k: call(*('wmiir','setsid','dolphin'),background=True)),
	('XF86AudioPlay',"MPD play/pause",
		lambda k: call(*('wmiir','setsid','mpc toggle'),background=True)),
	))
