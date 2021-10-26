from psychopy import visual, core, event, data, gui, parallel
import numpy as np
import random, os, pygame
from touchcomm import *

# -- GET INPUT FROM THE EXPERIMENTER --

exptInfo = {'01. Participant Pair Code':'test',
            '02. Number of repeats':10,
            '03. Response timout (sec)':7,
            '04. Receiver Screen':0,
            '05. Receiver screen resolution':'800,600', #'1280,720',
            '06. Folder for saving data':'data'}
exptInfo['07. Date and time']= data.getDateStr(format='%Y-%m-%d_%H-%M-%S') ##add the current time

dlg = gui.DlgFromDict(exptInfo, title='Experiment details', fixed=['07. Date and time'])
if dlg.OK:
    pass ## continue
else:
    core.quit() ## the user hit cancel so exit

receiverScreenRes = [int(i) for i in exptInfo['05. Receiver screen resolution'].split(',')]

# ----


# -- SETUP STIMULUS RANDOMISATION AND CONTROL --

items = ['attention','gratitude','love','sadness','happiness','calming']
toucherCueText = dict((line.strip().split('\t') for line in file('toucher-cues.txt')))
receiverCueText = dict((line.strip().split('\t') for line in file('receiver-cues.txt')))

stimList = []
for cue in items: stimList.append({'cue':cue})
trials = data.TrialHandler(stimList, exptInfo['02. Number of repeats'])
trials.data.addDataType('response')
correctText = ['incorrect','correct']

# ----

# -- MAKE FOLDER/FILES TO SAVE DATA --

dataFolder = './'+exptInfo['06. Folder for saving data']+'/'
if not os.path.exists(dataFolder):
    os.makedirs(dataFolder)

fileName = dataFolder + exptInfo['07. Date and time'] +'_'+ exptInfo['01. Participant Pair Code']
infoFile = open(fileName+'_info.csv', 'w') 
for k,v in exptInfo.iteritems(): infoFile.write(k + ',' + str(v) + '\n')
infoFile.close()
dataFile = open(fileName+'_communication-data.csv', 'w')
dataFile.write('trial,cued,response,correct\n')
logFile = open(fileName+'_log.csv', 'w')
logFile.write('time,event\n')

# ----

# -- SETUP INTERFACE --

outlineColour = [-1,-1,-1]
textColour = [-1,-1,-1]
startMessage = 'Please concentrate and lie still.'
waitMessage = '+'
finishedMessage = 'The session has finished. Thank you!'

receiverWin = visual.Window(fullscr = False, 
                            allowGUI = False, 
                            screen = exptInfo['04. Receiver Screen'],
                            size = receiverScreenRes)

receiverMessage = visual.TextStim(receiverWin,
                                    text = '',
                                    height = 0.15,
                                    color = textColour,
                                    units = 'norm',
                                    pos = (0,-0))

buttonWidth = 0.65
buttonHeight = 0.4
buttonColour = [0,.25,.9]
nCol = 2
nRow = 3

#evenly space the buttons from each other and edges
xpos = np.linspace(-1,1,nCol+2)[1:nCol+1]
ypos = -np.linspace(-1,1,nRow+2)[1:nRow+1]
buttonPosition = []
for x in xpos:
    for y in ypos:
        buttonPosition += [(x,y)]

receiverButton = []
receiverButtonText = []
for n in range(len(items)):
    
    receiverButton += [visual.Rect(receiverWin,
                            width = buttonWidth,
                            height= buttonHeight,
                            fillColor = buttonColour,
                            lineColor = outlineColour,
                            units = 'norm',
                            pos = buttonPosition[n])]
    
    receiverButtonText += [visual.TextStim(receiverWin,
                            text=receiverCueText[items[n]],
                            height=buttonHeight/4.5,
                            wrapWidth = buttonWidth,
                            color = textColour,
                            units = 'norm',
                            pos = buttonPosition[n])]

# -----

# -- SETUP AUDIO --

pygame.mixer.pre_init() 
pygame.mixer.init()
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

# wait for MRI trigger
receiverMessage.text = startMessage
receiverMessage.autoDraw = True
event.clearEvents()
receiverWin.flip()
MRItriggerReceived = False
while not MRItriggerReceived:
    receiverWin.flip()
    for (key,keyTime) in event.getKeys(['t','escape'], timeStamped=exptClock):
        if key in ['escape']:
            logEvent(keyTime,'experiment aborted',logFile)
            dataFile.close(); logFile.close(); core.quit()
        if key in ['t']:
            exptClock.add(keyTime)
            logEvent(0,'fMRI trigger',logFile)
            MRItriggerReceived = True

# start the experiment
for thisTrial in trials:
    
    # get the cue for this trial
    if trials.thisRepN % 10 == 0:
        suffix = ' - long' ## 1st in every 10
    else:
        suffix = ' - short' ## 2nd-10th in every 10
    thisCue = pygame.mixer.Sound('./sounds/{}{}.wav' .format(thisTrial['cue'],suffix))
    
    # show fixation cross to receiver
    receiverMessage.text = waitMessage
    receiverMessage.autoDraw = True
    event.clearEvents()
    ##logEvent(exptClock.getTime(),'fixation cross',logFile)
    receiverWin.flip()
    
    # cue toucher to perform touch
    logEvent(exptClock.getTime(),'toucher cue {}{}' .format(thisTrial['cue'],suffix),logFile)
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
    logEvent(exptClock.getTime() - stopDuration,'stop touching',logFile)
    
    # present cue buttons for receiver to make a choice
    receiverMessage.autoDraw = False ## turn off fixation cross
    ## randomise button positions
    randomItems = random.sample(items,len(items))
    for n, item in enumerate(items):
        receiverButtonText[n].text = receiverCueText[randomItems[n]]
        receiverButton[n].autoDraw = True
        receiverButtonText[n].autoDraw = True
    event.clearEvents()
    logEvent(exptClock.getTime(),'buttons presented',logFile)
    receiverWin.flip()
    
    # get response from receiver
    (responseN,rTime) = getSelection(receiverWin,receiverButton,exptInfo['03. Response timout (sec)'],exptClock)
    if responseN == -2:
        logEvent(rTime,'experiment aborted',logFile)
        dataFile.close(); logFile.close(); core.quit()
    elif responseN == -1:
        response = 'timeout'
    else:
        response = randomItems[responseN]
    logEvent(rTime,'receiver responded {} - {}' .format(response, correctText[int(thisTrial['cue']==response)]),logFile)
    
    # stop drawing buttons for receiver
    for n, item in enumerate(items):
        receiverButtonText[n].autoDraw = False
        receiverButton[n].autoDraw = False
    event.clearEvents()
    receiverWin.flip()
    
    trials.data.add('response',response)
    dataFile.write('{},{},{},{}\n' .format(trials.thisN+1,
                                                thisTrial['cue'],
                                                response,
                                                int(thisTrial['cue']==response)))
    logEvent(exptClock.getTime(),'{} of {} complete' .format(trials.thisN+1, trials.nTotal),logFile)

# -----

# prompt at the end of the experiment
receiverMessage.text = finishedMessage
event.clearEvents()
receiverMessage.draw()
logEvent(exptClock.getTime(),'experiment finished',logFile)
receiverWin.flip()
core.wait(2)
dataFile.close(); logFile.close()
receiverWin.close()
core.quit()
