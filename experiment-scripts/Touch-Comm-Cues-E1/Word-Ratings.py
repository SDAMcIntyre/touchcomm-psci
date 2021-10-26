from psychopy import visual, core, event, data, gui
import random, os
from touchcomm import getTextResponse

# -- GET INPUT FROM THE EXPERIMENTER --

exptInfo = {'01. Participant Code':'00', 
            '02. Role order':('toucher-then-receiver','receiver-then-toucher'),
            '03. Screen':1,
            '04. Screen resolution':'1919,1200',
            '05. Folder for saving data':'data'}
exptInfo['06. Date and time']= data.getDateStr(format='%Y-%m-%d_%H-%M-%S') ##add the current time

dlg = gui.DlgFromDict(exptInfo, title='Experiment details', fixed=['06. Date and time'])
if dlg.OK:
    pass ## continue
else:
    core.quit() ## the user hit cancel so exit

screenRes = [int(i) for i in exptInfo['04. Screen resolution'].split(',')]

# ----


# -- MAKE FOLDER/FILES TO SAVE DATA --

dataFolder = './'+exptInfo['05. Folder for saving data']+'/'
if not os.path.exists(dataFolder):
    os.makedirs(dataFolder)

fileName = dataFolder + exptInfo['06. Date and time'] + '_P' + exptInfo['01. Participant Code'] + '_' + exptInfo['02. Role order']
dataFile = open(fileName+'_rating-data.csv', 'w')
dataFile.write('cue,toucher-rating,receiver-rating\n')

# ----


# -- SETUP INTERFACE --

cues = ['GRATITUDE','LOVE','SADNESS','HAPPINESS','ATTENTION','CALMING']
cues = random.sample(cues,len(cues))

win = visual.Window(fullscr=False, 
                            allowGUI = False, 
                            screen = exptInfo['03. Screen'],
                            size = screenRes)
mouse = event.Mouse(win = win)

barMarker = visual.TextStim(win, text='|', units='norm')
toucherVAS = visual.RatingScale(win, low=-10, high=10, precision=10, 
    showValue=False, marker=barMarker, scale = '',
    tickHeight=0, stretch=1.5, labels=["not at all", "very much"],
    mouseOnly = True, pos= (0,0.4))
toucherPrompt = visual.TextStim(win, text='', height=.08, units='norm', pos=(0,0.6))

receiverVAS = visual.RatingScale(win, low=-10, high=10, precision=10, 
    showValue=False, marker=barMarker, scale = '',
    tickHeight=0, stretch=1.5, labels=["not at all", "very much"],
    mouseOnly = True, pos= (0,-0.4))
receiverPrompt = visual.TextStim(win, text='', height=.08, units='norm', pos=(0,-0.2))


textBoxWidth = 1.5
textBoxHeight = 0.4
textBoxPosition = (0,-0.2)
textBoxPromptPosition = (0, textBoxHeight/2 + textBoxPosition[1] + 0.1)
submitButtonPosition = (0, -textBoxHeight/2 + textBoxPosition[1] - 0.1)

textBox = visual.Rect(win,
                    width = textBoxWidth,
                    height = textBoxHeight,
                    fillColor = [1,1,1],
                    lineColor = [-1,-1,-1],
                    units = 'norm',
                    pos = textBoxPosition)

textBoxPrompt = visual.TextStim(win,
                    text = '',
                    height = 0.08,
                    wrapWidth = textBoxWidth,
                    color = [-1,-1,-1],
                    units = 'norm',
                    pos = textBoxPromptPosition)

textBoxResponse = visual.TextStim(win,
                    text = '',
                    height = 0.06,
                    wrapWidth = textBoxWidth,
                    color = [-1,-1,-1],
                    units = 'norm',
                    pos =  textBoxPosition)

submitButton = visual.Rect(win,
                    width = 0.2,
                    height = 0.1,
                    fillColor = [-0.5,-0.5,-0.5],
                    lineColor = [-1,-1,-1],
                    units = 'norm',
                    pos = submitButtonPosition)

submitButtonText = visual.TextStim(win,
                    text = 'Submit',
                    height = 0.08,
                    wrapWidth = 0.2,
                    color = [1,1,1],
                    units = 'norm',
                    pos = submitButtonPosition)

# ----

#-- RUN THE QUESTIONNAIRE --

for n, cue in enumerate(cues):
    toucherVAS.reset()
    receiverVAS.reset()
    toucherPrompt.text = 'In general, how much do you want to communicate {} to other people through touch?' .format(cue)
    receiverPrompt.text = 'In general, how much do you want others to communiate {} to you through touch?' .format(cue)
    event.clearEvents()
    win.flip()
    while toucherVAS.noResponse or receiverVAS.noResponse:
        receiverVAS.draw()
        receiverPrompt.draw()
        toucherVAS.draw()
        toucherPrompt.draw()
        win.flip()
        if event.getKeys(['escape']):
            core.quit()
    dataFile.write('{},{},{}\n' .format(cue,toucherVAS.getRating(),receiverVAS.getRating()))

win.flip()
core.wait(0.5)

textBox.autoDraw = True
textBoxPrompt.autoDraw = True
submitButton.autoDraw = True
submitButtonText.autoDraw = True
textBoxResponse.autoDraw = True

textBoxPrompt.text = 'In general, is there anything else you would like to communicate to other people through touch?'
event.clearEvents()
win.flip()
otherToucher = getTextResponse(win,mouse,submitButton,textBoxResponse)

textBoxPrompt.text = ''
textBoxResponse.text = ''
event.clearEvents()
win.flip()
core.wait(0.5)

textBoxPrompt.text = 'In general, is there anything else you would like others to communicate to you through touch?'
event.clearEvents()
win.flip()
otherReceiver = getTextResponse(win,mouse,submitButton,textBoxResponse)

dataFile.write('other,{},{}\n' .format(otherToucher,otherReceiver))

win.close()
dataFile.close()
core.quit()