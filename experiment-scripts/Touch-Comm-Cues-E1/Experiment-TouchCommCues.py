from psychopy import visual, core, event, data, gui, parallel
import numpy as np
import random, os, pygame
from touchcomm import *

# -- GET INPUT FROM THE EXPERIMENTER --

exptInfo = {'01. Toucher Participant Code':'00',
            '02. Receiver Participant Code':'00',
            '03. Number of repeats':5,
            '04. Toucher Screen':1,
            '05. Toucher screen resolution':'1919,1200',
            '06. Receiver Screen':0,
            '07. Receiver screen resolution':'1920,1200', #'1280,720',
            '08. Include open ended question before first presentation':True,
            '09. Play audio cue for video sync':True,
            '10. Send parallel port signal for fEMG sync':True,
            '11. Parallel port address':'0x3FF8',
            '12. Folder for saving data':'data'}
exptInfo['13. Date and time']= data.getDateStr(format='%Y-%m-%d_%H-%M-%S') ##add the current time

dlg = gui.DlgFromDict(exptInfo, title='Experiment details', fixed=['13. Date and time'])
if dlg.OK:
    pass ## continue
else:
    core.quit() ## the user hit cancel so exit

toucherScreenRes = [int(i) for i in exptInfo['05. Toucher screen resolution'].split(',')]
receiverScreenRes = [int(i) for i in exptInfo['07. Receiver screen resolution'].split(',')]

# ----


# -- SETUP STIMULUS RANDOMISATION AND CONTROL --

toucherItem = ['gratitude','love','sadness','happiness','attention','calming']
receiverItem = ['other'] + toucherItem
toucherCueText = dict((line.strip().split('\t') for line in file('toucher-cues.txt')))
receiverCueText = dict((line.strip().split('\t') for line in file('receiver-cues.txt')))

stimList = []
for cue in toucherItem: stimList.append({'cue':cue})
# extra set if using open ended questions:
nReps = exptInfo['03. Number of repeats'] + exptInfo['08. Include open ended question before first presentation']
trials = data.TrialHandler(stimList, nReps)
trials.data.addDataType('response')

# ----

# -- MAKE FOLDER/FILES TO SAVE DATA --

dataFolder = './'+exptInfo['12. Folder for saving data']+'/'
if not os.path.exists(dataFolder):
    os.makedirs(dataFolder)

fileName = dataFolder + exptInfo['13. Date and time'] + '_T' + exptInfo['01. Toucher Participant Code']  + '_R' + exptInfo['02. Receiver Participant Code']
infoFile = open(fileName+'_info.csv', 'w') 
for k,v in exptInfo.iteritems(): infoFile.write(k + ',' + str(v) + '\n')
dataFile = open(fileName+'_communication-data.csv', 'w')
dataFile.write('trial,cued,response,other-text,correct\n')

# ----

# -- SETUP AUDIO --

if exptInfo['09. Play audio cue for video sync']:
    pygame.mixer.pre_init() 
    pygame.mixer.init()
    audioCue = pygame.mixer.Sound('cue.wav')

# ----

# -- SETUP PARALLEL PORT COMMUNICATION --

if exptInfo['10. Send parallel port signal for fEMG sync']:
    port = parallel.ParallelPort(address=exptInfo['11. Parallel port address'])
    port.setData(0)

# ----

# -- SETUP INTERFACES --

outlineColour = [-1,-1,-1]
textColour = [-1,-1,-1]
waitMessage = 'Please wait...'
finishedMessage = 'The session has finished. Thank you!'

# toucher interface

toucherWin = visual.Window(fullscr=False, 
                            allowGUI = False, 
                            screen = exptInfo['04. Toucher Screen'],
                            size = toucherScreenRes)
toucherMouse = event.Mouse(win = toucherWin)

goColour = [0,1,0.1]
stopColour = [1,0,0.1]
toucherCue = visual.TextStim(toucherWin,
                                text='',
                                height=0.08,
                                color = textColour,
                                units = 'norm',
                                pos = (0,0.2))

toucherButton = visual.Circle(toucherWin,
                                radius = 0.12,
                                lineColor = outlineColour,
                                units = 'norm',
                                pos = (0,-0.2))

toucherButtonText = visual.TextStim(toucherWin,
                                    text ='',
                                    height = 0.08,
                                    color = textColour,
                                    units = 'norm',
                                    pos = (0,-0.2))

toucherMessage = visual.TextStim(toucherWin,
                                    text = '',
                                    height = 0.08,
                                    color = textColour,
                                    units = 'norm',
                                    pos = (0,-0))


# receiver interface

receiverWin = visual.Window(fullscr = True, 
                            allowGUI = False, 
                            screen = exptInfo['06. Receiver Screen'],
                            size = receiverScreenRes)
receiverMouse = event.Mouse(win = receiverWin)

buttonWidth = 0.6
buttonHeight = 0.2
buttonColour = [0,.25,.9]
nCol = 2
nRow = 4

##evenly space the buttons from each other and edges
xpos = np.linspace(-1,1,nCol+2)[1:nCol+1]
ypos = np.linspace(-1,1,nRow+2)[1:nRow+1]
buttonPosition = [(0,ypos[0])]
for x in xpos:
    for y in ypos[1:nRow]:
        buttonPosition += [(x,y)]


receiverButton = []
receiverButtonText = []
for n in range(len(receiverItem)):
    
    receiverButton += [visual.Rect(receiverWin,
                            width = buttonWidth,
                            height= buttonHeight,
                            fillColor = buttonColour,
                            lineColor = outlineColour,
                            units = 'norm',
                            pos = buttonPosition[n])]
    
    receiverButtonText += [visual.TextStim(receiverWin,
                            text=receiverCueText[receiverItem[n]],
                            height=buttonHeight/3,
                            wrapWidth = buttonWidth,
                            color = textColour,
                            units = 'norm',
                            pos = buttonPosition[n])]

textBoxWidth = 1.5
textBoxHeight = 0.4
textBoxPosition = (0,-0.2)
textBoxPromptPosition = (0, textBoxHeight/2 + textBoxPosition[1] + 0.1)
submitButtonPosition = (0, -textBoxHeight/2 + textBoxPosition[1] - 0.1)
textBox = visual.Rect(receiverWin,
                        width = textBoxWidth,
                        height = textBoxHeight,
                        fillColor = [1,1,1],
                        lineColor = outlineColour,
                        units = 'norm',
                        pos = textBoxPosition)

textBoxPrompt = visual.TextStim(receiverWin,
                        text = 'What do you think your partner was trying to communicate?',
                        height = 0.08,
                        wrapWidth = textBoxWidth,
                        color = textColour,
                        units = 'norm',
                        pos = textBoxPromptPosition)

textBoxResponse = visual.TextStim(receiverWin,
                        text = '',
                        height = 0.06,
                        wrapWidth = textBoxWidth,
                        color = textColour,
                        units = 'norm',
                        pos =  textBoxPosition)

submitButton = visual.Rect(receiverWin,
                        width = 0.2,
                        height = 0.1,
                        fillColor = [-0.5,-0.5,-0.5],
                        lineColor = outlineColour,
                        units = 'norm',
                        pos = submitButtonPosition)

submitButtonText = visual.TextStim(receiverWin,
                        text = 'Submit',
                        height = 0.08,
                        wrapWidth = 0.2,
                        color = [1,1,1],
                        units = 'norm',
                        pos = submitButtonPosition)

receiverMessage = visual.TextStim(receiverWin,
                                    text = '',
                                    height = 0.08,
                                    color = textColour,
                                    units = 'norm',
                                    pos = (0,-0))

# -----


# -- RUN THE EXPERIMENT --

for thisTrial in trials:
    
    print('Cued item: {}' .format(thisTrial['cue']))
    toucherCue.text = toucherCueText[thisTrial['cue']]
    otherText = ''
    textBoxResponse.text = otherText
    
    # present cue to toucher
    toucherButton.fillColor = goColour
    toucherButtonText.text = 'GO'
    toucherCue.autoDraw = True
    toucherButton.autoDraw = True
    toucherButtonText.autoDraw = True
    receiverMessage.text = waitMessage
    receiverMessage.autoDraw = True
    event.clearEvents()
    toucherWin.flip()
    receiverWin.flip()
    core.wait(1)
    clicked = False
    while not clicked:
        if event.getKeys(['escape']):
            print 'user aborted'
            dataFile.close()
            core.quit()
        if event.getKeys(['return']) or toucherMouse.isPressedIn(toucherButton):
                clicked = True
    
    # data sync cues
    if exptInfo['09. Play audio cue for video sync']:
        soundCh = audioCue.play()
        while soundCh.get_busy():
            pass
    
    if exptInfo['10. Send parallel port signal for fEMG sync']:
        stimNumber = toucherItem.index(thisTrial['cue']) + 1
        port.setData(stimNumber)
    
    # wait for toucher to finish
    toucherButton.fillColor = stopColour
    toucherButtonText.text = 'STOP'
    event.clearEvents()
    receiverMessage.text = ''
    toucherWin.flip()
    receiverWin.flip()
    core.wait(1)
    clicked = False
    while not clicked:
        if event.getKeys(['escape']):
            print 'user aborted'
            dataFile.close()
            core.quit()
        if event.getKeys(['return']) or toucherMouse.isPressedIn(toucherButton):
                clicked = True
    
    # wait for receiver to respond
    toucherCue.autoDraw = False
    toucherButton.autoDraw = False
    toucherButtonText.autoDraw = False
    toucherMessage.text = waitMessage
    toucherMessage.autoDraw = True
    receiverMessage.autoDraw = False
    event.clearEvents()
    toucherWin.flip()
    receiverWin.flip()
    
    if exptInfo['10. Send parallel port signal for fEMG sync']:
        port.setData(0)
    
    if exptInfo['08. Include open ended question before first presentation'] and trials.thisRepN == 0:
        response = 'open'
        textBox.autoDraw = True
        textBoxPrompt.autoDraw = True
        submitButton.autoDraw = True
        submitButtonText.autoDraw = True
        textBoxResponse.autoDraw = True
        event.clearEvents()
        toucherWin.flip()
        receiverWin.flip()
        otherText = getTextResponse(receiverWin,receiverMouse,submitButton,textBoxResponse)
        textBox.autoDraw = False
        textBoxPrompt.autoDraw = False
        submitButton.autoDraw = False
        submitButtonText.autoDraw = False
        textBoxResponse.autoDraw = False
        receiverWin.flip()
    
    else:
        # present cue buttons for receiver to make a choice
        ## randomise button positions except for 'other'
        randomButtonPosition = [buttonPosition[0]] + random.sample(buttonPosition[1:len(buttonPosition)],len(buttonPosition)-1)
        for n in range(len(receiverItem)):
            receiverButton[n].pos = randomButtonPosition[n]
            receiverButton[n].autoDraw = True
            receiverButtonText[n].autoDraw = True
            receiverButtonText[n].pos = randomButtonPosition[n]
        event.clearEvents()
        toucherWin.flip()
        receiverWin.flip()
        
        clicked = getButtonClick(receiverWin,receiverMouse,receiverButton)
        response = receiverItem[clicked]
        
        for n in range(len(receiverItem)):
            receiverButton[n].autoDraw = False
            receiverButtonText[n].autoDraw = False
        event.clearEvents()
        receiverWin.flip()
        
        # if the receiver chose 'other' get text input
        if response == 'other':
            textBox.autoDraw = True
            textBoxPrompt.autoDraw = True
            submitButton.autoDraw = True
            submitButtonText.autoDraw = True
            textBoxResponse.autoDraw = True
            event.clearEvents()
            receiverWin.flip()
            otherText = getTextResponse(receiverWin,receiverMouse,submitButton,textBoxResponse)
            textBox.autoDraw = False
            textBoxPrompt.autoDraw = False
            submitButton.autoDraw = False
            submitButtonText.autoDraw = False
            textBoxResponse.autoDraw = False
            receiverWin.flip()
    
    toucherMessage.text = ''
    toucherMessage.autoDraw = False
    event.clearEvents()
    toucherWin.flip()
    
    print('Response: {}' .format(response))
    if response == 'other' or response == 'open':
        print(otherText)
    trials.data.add('response',response)
    dataFile.write('{},{},{},{},{}\n' .format(trials.thisN+1,
                                                thisTrial['cue'],
                                                response,
                                                '"' + otherText + '"',
                                                int(thisTrial['cue']==response)))
    print('{} of {} complete.\n' .format(trials.thisN+1, trials.nTotal))

# -----

## prompt at the end of the experiment
toucherMessage.text = finishedMessage
receiverMessage.text = finishedMessage
event.clearEvents()
toucherMessage.draw()
receiverMessage.draw()
toucherWin.flip()
receiverWin.flip()
core.wait(2)
dataFile.close()
toucherWin.close()
receiverWin.close()
core.quit()
