from psychopy import visual, core, event, data, gui
import numpy as np
import random, os, pygame
from touchcomm import *


# -- GET INPUT FROM THE EXPERIMENTER --

exptInfo = {'01. Participant Code':'test',
            '02. Number of repeats':10,
            '03. Press to continue':True,
            '04. Participant screen':1,
            '05. Participant screen resolution':'1280,720',
            '06. Experimenter screen':0,
            '07. Experimenter screen resolution':'1280,720',
            '08. Play audio cue for video sync':True,
            '09. Folder for saving data':'data'}
exptInfo['10. Date and time']= data.getDateStr(format='%Y-%m-%d_%H-%M-%S') ##add the current time

dlg = gui.DlgFromDict(exptInfo, title='Experiment details', fixed=['10. Date and time'])
if dlg.OK:
    pass ## continue
else:
    core.quit() ## the user hit cancel so exit

# text displayed to experimenter and participant
if exptInfo['08. Play audio cue for video sync']:
    startMessage = 'Make sure speakers are plugged in, not headphones. Then press space to start. Plug in headphones after the video sync sound has played.'
else:
    startMessage = 'Press Space to start.'
waitMessage = 'Please wait.'
continueMessage = 'Press space for the audio cue.'
touchMessage = 'Follow the audio cue.'
fixationMessage = '+'
finishedMessage = 'The session has finished.'

receiverScreenRes = [int(i) for i in exptInfo['05. Participant screen resolution'].split(',')]
toucherScreenRes = [int(i) for i in exptInfo['07. Experimenter screen resolution'].split(',')]

# ----


# -- SETUP STIMULUS RANDOMISATION AND CONTROL --

items = ['attention','gratitude','love','sadness','happiness','calming']
receiverItem = ['other'] + items
receiverCueText = dict((line.strip().split('\t') for line in file('receiver-cues7.txt')))
toucherCueText = dict((line.strip().split('\t') for line in file('toucher-cues.txt')))

stimList = []
for cue in items: stimList.append({'cue':cue})
trials = data.TrialHandler(stimList, exptInfo['02. Number of repeats'])
trials.data.addDataType('response')
correctText = ['incorrect','correct']

# ----

# -- MAKE FOLDER/FILES TO SAVE DATA --

dataFolder = './'+exptInfo['09. Folder for saving data']+'/'
if not os.path.exists(dataFolder):
    os.makedirs(dataFolder)

fileName = dataFolder + 'touch-comm-Behav_' + exptInfo['10. Date and time'] +'_P' + exptInfo['01. Participant Code']
infoFile = open(fileName+'_info.csv', 'w') 
for k,v in exptInfo.iteritems(): infoFile.write(k + ',' + str(v) + '\n')
infoFile.close()
dataFile = open(fileName+'_communication-data.csv', 'w')
dataFile.write('trial,cued,response, other-text,correct\n')
logFile = open(fileName+'_log.csv', 'w')
logFile.write('time,event\n')

# ----

# -- SETUP INTERFACE --

outlineColour = [-1,-1,-1]
textColour = [-1,-1,-1]

toucherWin = visual.Window(fullscr = False, 
                            allowGUI = False, 
                            screen = exptInfo['06. Experimenter screen'],
                            size = toucherScreenRes)

toucherMessage = visual.TextStim(toucherWin,
                                    text = '',
                                    height = 0.15,
                                    color = textColour,
                                    units = 'norm',
                                    pos = (0,-0))


receiverWin = visual.Window(fullscr = True, 
                            allowGUI = False, 
                            screen = exptInfo['04. Participant screen'],
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

# -- SETUP AUDIO --

pygame.mixer.pre_init() 
pygame.mixer.init()
audioSync = pygame.mixer.Sound('./sounds/sync.wav')
thisCue = pygame.mixer.Sound('./sounds/attention.wav')
goStopCue = pygame.mixer.Sound('./sounds/go-stop.wav')
# durations within the audio file:
silentLead = 0.064
countDownDuration = 3.0
stopDuration = 0.434

# ----

# -- RUN THE EXPERIMENT --

exptClock=core.Clock()
exptClock.reset()

# wait for start trigger
toucherMessage.text = startMessage
toucherMessage.autoDraw = True
receiverMessage.text = waitMessage
receiverMessage.autoDraw = True
event.clearEvents()
toucherWin.flip()
receiverWin.flip()
startTriggerReceived = False
while not startTriggerReceived:
    receiverWin.flip()
    for (key,keyTime) in event.getKeys(['space','escape'], timeStamped=exptClock):
        receiverMouse.clickReset()
        if key in ['escape']:
            logEvent(keyTime,'experiment aborted',logFile)
            dataFile.close(); logFile.close(); core.quit()
        if key in ['space']:
            exptClock.add(keyTime)
            receiverMouse.clickReset()
            logEvent(0,'experiment started',logFile)
            startTriggerReceived = True
if exptInfo['08. Play audio cue for video sync']:
    soundCh = audioSync.play()
    while soundCh.get_busy():
        pass


# start the experiment
for thisTrial in trials:
    
    event.clearEvents()
    
    # get the cue for this trial
    thisCue = pygame.mixer.Sound('./sounds/{} - short.wav' .format(thisTrial['cue']))
    
    # wait for experimenter
    receiverMessage.text = waitMessage
    receiverWin.flip()
    
    # display cue to experimenter
    if not exptInfo['03. Press to continue']:
        continuePressed = True
        toucherMessage.text = toucherCueText[thisTrial['cue']]
        toucherWin.flip()
        core.wait(3)
        logEvent(exptClock.getTime(),'experimenter waited for cue',logFile)
    else:
        continuePressed = False
        toucherMessage.text = toucherCueText[thisTrial['cue']] + '.\n' + continueMessage
        toucherWin.flip()
        while not continuePressed:
            for (key,keyTime) in event.getKeys(['space','escape'], timeStamped=exptClock):
                if key in ['escape']:
                    logEvent(keyTime,'experiment aborted',logFile)
                    dataFile.close(); logFile.close(); core.quit()
                if key in ['space']:
                    logEvent(keyTime,'experimenter pressed for cue',logFile)
                    continuePressed = True
    
    # show fixation cross to receiver
    receiverMessage.text = fixationMessage
    receiverWin.flip()
    
    # audio cue for toucher
    toucherMessage.text = toucherCueText[thisTrial['cue']] + '.\n'+ touchMessage
    toucherWin.flip()
    logEvent(exptClock.getTime(),'toucher cue {}' .format(thisTrial['cue']),logFile)
    soundCh = thisCue.play()
    while soundCh.get_busy():
        for (key,keyTime) in event.getKeys(['escape'], timeStamped=exptClock):
            soundCh.stop()
            logEvent(keyTime,'experiment aborted',logFile)
            dataFile.close(); logFile.close(); core.quit()
    
    countDownStartTime = exptClock.getTime()
    soundCh = goStopCue.play()
    logEvent(countDownStartTime + silentLead,'countdown to touch',logFile)
    logEvent(countDownStartTime + silentLead + countDownDuration,'start touching',logFile)
    while soundCh.get_busy():
        for (key,keyTime) in event.getKeys(['escape'], timeStamped=exptClock):
            soundCh.stop()
            logEvent(keyTime,'experiment aborted',logFile)
            dataFile.close(); logFile.close(); core.quit()
            signalNo = items.index(thisTrial['cue'])

    
    logEvent(exptClock.getTime() - stopDuration,'stop touching',logFile)
    
    # wait for participant
    toucherMessage.text = waitMessage
    toucherWin.flip()
    
    # present cue buttons for receiver to make a choice
    otherText = ''
    textBoxResponse.text = otherText
    receiverMessage.text = '' 
    ## randomise button positions
    randomItems = random.sample(receiverItem,len(receiverItem))
    for n, item in enumerate(receiverItem):
        receiverButtonText[n].text = receiverCueText[randomItems[n]]
        receiverButton[n].autoDraw = True
        receiverButtonText[n].autoDraw = True
    logEvent(exptClock.getTime(),'buttons presented',logFile)
    receiverWin.flip()
    
     # get response from receiver
    (responseN,rTime) = getButtonClick(receiverWin,receiverMouse,receiverButton,exptClock)
    if responseN == -2:
        logEvent(rTime,'experiment aborted',logFile)
        dataFile.close(); logFile.close(); core.quit()
    elif responseN == -1:
        response = 'timeout'
    
    # if the receiver chose 'other' get text input
    elif randomItems[responseN] == 'other':
        response = randomItems[responseN]
        for n, item in enumerate(receiverItem):
            receiverButtonText[n].autoDraw = False
            receiverButton[n].autoDraw = False
        receiverWin.flip()
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
        receiverWin.flip

    else:
        response = randomItems[responseN]
        logEvent(rTime,'receiver responded {} - {}' .format(response, correctText[int(thisTrial['cue']==response)]),logFile)


# stop drawing buttons for receiver
    for n, item in enumerate(receiverItem):
        receiverButtonText[n].autoDraw = False
        receiverButton[n].autoDraw = False
    receiverWin.flip()
    
    trials.data.add('response',response)
    dataFile.write('{},{},{},{},{}\n' .format(trials.thisN+1,
                                                thisTrial['cue'],
                                                response,
                                                '"' + otherText + '"',
                                                int(thisTrial['cue']==response)))
    logEvent(exptClock.getTime(),'{} of {} complete' .format(trials.thisN+1, trials.nTotal),logFile)

# -----

# prompt at the end of the experiment
event.clearEvents()
receiverMessage.text = finishedMessage
if exptInfo['08. Play audio cue for video sync']:
    toucherMessage.text = finishedMessage + '\n Unplug the headphones, then press space to play the video sync sound.'
else:
    toucherMessage.text = finishedMessage
toucherWin.flip()
receiverWin.flip()

if exptInfo['08. Play audio cue for video sync']:
    endTriggerReceived = False
    while not endTriggerReceived:
        toucherWin.flip()
        receiverWin.flip()
        for (key,keyTime) in event.getKeys(['space'], timeStamped=exptClock):
            receiverMouse.clickReset()
            if key in ['space']:
                endTriggerReceived = True



logEvent(exptClock.getTime(),'experiment finished',logFile)
core.wait(2)
dataFile.close(); logFile.close()
receiverWin.close()
toucherWin.close()
core.quit()
