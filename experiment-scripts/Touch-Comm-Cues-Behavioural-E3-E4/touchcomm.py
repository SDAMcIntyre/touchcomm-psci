from psychopy import visual, event, core
import numpy as np
import random, time

def getSelection(win,buttonList,timeout,clock):
    confirmed = False
    aborted = False
    fwd = ['a','down']
    bwd = ['b','up']
    conf = ['c','return']
    quit = ['escape']
    buttonSelected = -1
    response = -1
    countDown = core.CountdownTimer()
    countDown.add(timeout)
    while not aborted and not confirmed and countDown.getTime() > 0:
        for (key,t) in event.getKeys(fwd+bwd+conf+quit, timeStamped=clock):
            if key in fwd:
                buttonList[buttonSelected].opacity = 1
                buttonSelected = (buttonSelected+1) % len(buttonList)
                buttonList[buttonSelected].opacity = 0.3
            if key in bwd:
                buttonList[buttonSelected].opacity = 1
                if buttonSelected < 0:
                    buttonSelected = buttonSelected % len(buttonList)
                else:
                    buttonSelected = (buttonSelected-1) % len(buttonList)
                buttonList[buttonSelected].opacity = 0.3
            if key in conf and buttonSelected >= 0:
                buttonList[buttonSelected].opacity = 1
                response = buttonSelected
                confirmed = True
            if key in quit:
                buttonList[buttonSelected].opacity = 1
                response = -2
                aborted = True
            win.flip()
    if countDown.getTime() <= 0: t = clock.getTime()
    buttonList[buttonSelected].opacity = 1
    win.flip()
    return (response,t)

def getButtonClick(win,mouse,buttonList,clock):
    clicked = False
    aborted = False
    while not clicked and not aborted:
        for n, button in enumerate(buttonList):
            if mouse.isPressedIn(button, buttons=[0]):
                mbutton, tList = mouse.getPressed(getTime=True)
                t = tList[0]
                clicked = True
                response = n
                break
            ## is the mouse inside the shape (hovering over it)?
            if button.contains(mouse):
                button.opacity = 0.3
            else:
                button.opacity = 1
            win.flip()
            time.sleep(0.001)
        for (key,t) in event.getKeys(['escape'], timeStamped=clock):
            response = -2
            aborted = True
    return (response,t)

def logEvent(time,event,logFile):
    logFile.write('{},{}\n' .format(time,event))
    print('LOG: {} {}' .format(time, event))

def ping(arduino, printMessages):
    arduinoSays = ''
    while not arduinoSays == 'ack':
        arduino.write('ping')
        arduinoSays = arduino.readline().strip()
        if printMessages and len(arduinoSays)> 0: print('arduino: {}' .format(arduinoSays))
    return arduinoSays

def send_signal(arduino, signalNo, printMessages):
    arduinoSays = ''
    while not arduinoSays == 'signal':
        arduino.write('signal')
        arduinoSays = arduino.readline().strip()
        if printMessages and len(arduinoSays)> 0: print('arduino: {}' .format(arduinoSays))
    signalSent = False
    while not signalSent:
        arduino.write(str(int(signalNo)))
        arduinoSays = arduino.readline().strip()
        if printMessages and len(arduinoSays)> 0: print('arduino: {}' .format(arduinoSays))
        if int(arduinoSays) == int(signalNo): signalSent = True
    return arduinoSays

if __name__ == "__main__":
    win = visual.Window(fullscr = True, allowGUI = False)
    exptClock = core.Clock()
    exptClock.reset()
    options = ['A','B','C','D','E','F']
    buttonWidth = 0.6
    buttonHeight = 0.2
    buttonColour = [0,.25,.9]
    buttonOpacity = 1
    nCol = 2
    nRow = 3

    ##evenly space the buttons from each other and edges
    xpos = np.linspace(-1,1,nCol+2)[1:nCol+1]
    ypos = -np.linspace(-1,1,nRow+2)[1:nRow+1]
    buttonPosition = []
    for x in xpos:
        for y in ypos:
            buttonPosition += [(x,y)]
    
    buttonBox = []
    buttonText = []
    for n, option in enumerate(options):
        
        buttonBox += [visual.Rect(win,
                                width = buttonWidth,
                                height = buttonHeight,
                                fillColor = buttonColour,
                                lineColor = [-1,-1,-1],
                                units = 'norm',
                                pos = buttonPosition[n])]
        
        buttonText += [visual.TextStim(win,
                                text = option,
                                height = buttonHeight/3,
                                wrapWidth = buttonWidth,
                                color = [-1,-1,-1],
                                units = 'norm',
                                pos = buttonPosition[n])]
    
    randomOptions = random.sample(options, len(options))
    for n, option in enumerate(randomOptions):
        buttonText[n].text = option
        buttonBox[n].autoDraw = True
        buttonText[n].autoDraw = True
    event.clearEvents()
    win.flip()
    clock = core.Clock()
    clock.reset()
    (response,rTime) = getSelection(win,buttonBox,20,clock)
    for n, option in enumerate(options):
        buttonBox[n].autoDraw = False
        buttonText[n].autoDraw = False
    event.clearEvents()
    win.flip()
    print rTime
    if response == -2:
        print 'aborted'
    elif response == -1:
        print 'timeout'
    else:
        print randomOptions[response]

def getTextResponse(win,mouse,submitButtonVisual,textVisual):
    alphanumLower = 'abcdefghijklmnopqrstuvwxyz1234567890'
    alphanumUpper = 'ABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890'
    ignoreKeys = ['escape','tab','return',
                    'lalt','ralt','lctrl','rctrl',
                    'lwindows','rwindows','less','menu',
                    'minus','plus','end','pagedown','pageup',
                    'home','up','down','left','right','insert',
                    'f1','f2','f3','f4','f5','f6','f7','f8',
                    'f9','f10','f11','f12']
    inputText = ''
    submitted = False
    capslock = False
    shift = False
    while not submitted:
        if mouse.isPressedIn(submitButtonVisual):
            submitted = True
        for key in event.getKeys():
            if key in ['delete','backspace']:
                inputText = inputText[:-1] ##delete last character
            elif key in ['rshift','lshift']:
                shift = True
            elif key in ['capslock']:
                capslock = 1 - capslock
            elif key in ['space']: 
                inputText += ' '
            elif key in ['period']:
                inputText += '.'
            elif key in ['comma']:
                inputText += ','
            elif key in ['apostrophe']:
                inputText += '\''
            elif key in ignoreKeys:
                pass ##do nothing
            elif key[0] in alphanumLower:
                if shift:
                    inputText += alphanumUpper[alphanumLower.index(key)]
                    shift = False
                elif capslock:
                    inputText += alphanumUpper[alphanumLower.index(key)]
                else:
                    inputText += key
            textVisual.text = inputText
            win.flip()
    return inputText

if __name__ == "__main__":
    win = visual.Window(fullscr = True, allowGUI = False)
    mouse = event.Mouse(win)
    
    options = ['other','A','B','C','D','E','F','G','H']
    buttonWidth = 0.6
    buttonHeight = 0.2
    buttonColour = [0,.25,.9]
    buttonOpacity = 1
    mouseoverOpacity = 0.3
    nCol = 2
    nRow = 5

    ##evenly space the buttons from each other and edges
    xpos = np.linspace(-1,1,nCol+2)[1:nCol+1]
    ypos = np.linspace(-1,1,nRow+2)[1:nRow+1]
    buttonPosition = [(0,ypos[0])]
    for x in xpos:
        for y in ypos[1:nRow]:
            buttonPosition += [(x,y)]
    
    button = []
    buttonText = []
    for n, option in enumerate(options):
        
        button += [visual.Rect(win,
                                width = buttonWidth,
                                height = buttonHeight,
                                fillColor = buttonColour,
                                lineColor = [-1,-1,-1],
                                units = 'norm',
                                pos = buttonPosition[n])]
        
        buttonText += [visual.TextStim(win,
                                text = option,
                                height = buttonHeight/3,
                                wrapWidth = buttonWidth,
                                color = [-1,-1,-1],
                                units = 'norm',
                                pos = buttonPosition[n])]
    
    for n, option in enumerate(options):
        button[n].autoDraw = True
        buttonText[n].autoDraw = True
    event.clearEvents()
    win.flip()
    clicked = getButtonClick(win,mouse,button)
    for n, option in enumerate(options):
        button[n].autoDraw = False
        buttonText[n].autoDraw = False
    event.clearEvents()
    win.flip()
    print options[clicked]

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
                        text = 'Type in some text:',
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

    textBox.autoDraw = True
    textBoxPrompt.autoDraw = True
    submitButton.autoDraw = True
    submitButtonText.autoDraw = True
    textBoxResponse.autoDraw = True
    event.clearEvents()
    win.flip()
    response = getTextResponse(win,mouse,submitButton,textBoxResponse)
    print response