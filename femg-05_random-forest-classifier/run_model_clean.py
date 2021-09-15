#https://www.tandfonline.com/doi/abs/10.1080/02533839.2013.799946
#was recorded at 500 Hz, our data was at 1 kHz

import numpy as np
import pandas as pd
import cPickle as pickle
import matplotlib.pyplot as plt

class Model:
    ATTENTION = 0
    HAPPINESS = 1
    CALMING = 2
    LOVE = 3
    GRATITUDE = 4
    SADNESS = 5

    def __init__(self):
        self.raw_data = None
        self.trials = []
        self.features = []
        self.labels = []
        self.gest2trial = {}
        self.comcsv = None
        self.response_labels = []
        self.correct = []
        self.relationships = []

    def load_text(self,infiles,designations,comfile):
        self.raw_data = []
        print('loading text...')
        for ix in range(len(infiles)):
            self.raw_data.append((designations[ix],pd.read_csv(infiles[ix])))
        self.comcsv = pd.read_csv(comfile).values
        print('loaded')

    def build_model(self):
        print('building model...')
        self.trials = []
        self.features = []
        self.labels = []
        self.gest2trial = {}
        self.response_labels = []
        self.correct = []
        self.csvrows = []
        self.relationships = []

        passed = 0
        passedT = 0
        passedR = 0

        #dat.ix[(dat['trial']==2) | (dat['trial']==4)]

        maxtrial = 0
        session_names = []
        for tup in self.raw_data:
            desig,dat = tup
            mt = np.max(dat['trial'])
            if mt > maxtrial:
                maxtrial = mt
            session_names.extend(np.unique(dat['session']).tolist())

        session_names = np.unique(session_names)
        for sn in session_names:
            for tnum in range(maxtrial):
                dat4all = True
                dat4toucher = True
                dat4receiver = True

                for tup in self.raw_data:
                    desig,dat = tup
                    if len(dat.ix[(dat['trial']==tnum) & (dat['session']==sn)]) == 0:
                        dat4all = False
                        if desig == 't_zyg' or desig == 't_cor':
                            dat4toucher = False
                        if desig == 'r_zyg' or desig == 'r_cor':
                            dat4receiver = False

                if dat4toucher:
                    passedT += 1
                    print('toucher: ', passedT)
                if dat4receiver:
                    passedR += 1
                    print('receiver: ', passedR)

                if (not dat4toucher) and (not dat4receiver):
                    continue

                else:
                    if dat4toucher and dat4receiver:
                        passed += 1
                        print('all4: ',passed)

                    trial = Trial(sn)
                    for tup in self.raw_data:
                        desig,dat=tup

                        if len(dat.ix[(dat['trial'] == tnum) & (dat['session'] == sn)]) == 0:
                            if desig == 't_zyg':
                                trial.t_zyg = None
                            elif desig == 't_cor':
                                trial.t_cor = None
                            elif desig == 'r_zyg':
                                trial.r_zyg = None
                            elif desig == 'r_cor':
                                trial.r_cor = None
                            else:
                                raise ValueError('an incorrect designator was found.')
                            continue

                        bldat = dat[(dat['session']==sn) & (dat['trial']==tnum) & (dat['time']<0)]
                        tdat = dat[(dat['session']==sn) & (dat['trial']==tnum) & (dat['time']>=0) & (dat['time']<1)]
                        print(sn,tnum)

                        ext = '_clean'

                        trial.t = tdat['time']
                        if desig == 't_zyg':
                            trial.t_zyg = tdat['tzyg'+ext] - np.mean(bldat['tzyg'+ext])
                        elif desig == 't_cor':
                            trial.t_cor = tdat['tcor'+ext] - np.mean(bldat['tcor'+ext])
                        elif desig == 'r_zyg':
                            trial.r_zyg = tdat['rzyg'+ext] - np.mean(bldat['rzyg'+ext])
                        elif desig == 'r_cor':
                            trial.r_cor = tdat['rcor'+ext] - np.mean(bldat['rcor'+ext])
                        else:
                            raise ValueError('an incorrect designator was found.')

                        print(sn)
                        print(dat[(dat['session']==sn) & (dat['trial']==tnum)]['cue'].iloc[0])
                        trial.label = dat[(dat['session']==sn) & (dat['trial']==tnum)]['cue'].iloc[0]

                    trial.number = long(tnum)
                    TR = trial.sessionID.split('_')
                    T = 'P' + TR[0].replace('T', '')
                    R = 'P' + TR[1].replace('R', '')
                    print(T,R,trial.number)
                    csvrow = np.where(
                        (self.comcsv[:, 0] == T) & (self.comcsv[:, 1] == R) & (self.comcsv[:, 3] == trial.number))[
                        0]
                    print(csvrow)
                    trial.response = self.comcsv[csvrow, 5]
                    trial.correct = self.comcsv[csvrow, 7]
                    print('check this: ',trial.label,trial.response, trial.correct)
                    #trial.relationship = self.comcsv[csvrow, 10]
                    trial.csvrow = csvrow

                    self.trials.append(trial)
                    if not trial.label in self.gest2trial.keys():
                        self.gest2trial[trial.label] = [trial]
                    else:
                        self.gest2trial[trial.label].append(trial)
                    self.features.append(self.get_features(trial))
                    self.labels.append(trial.label)
                    self.response_labels.append(trial.response)
                    self.correct.append(trial.correct)
                    self.csvrows.append(trial.csvrow)
                    self.relationships.append(trial.relationship)

        self.features = np.array(self.features, ndmin=2)
        self.csvrows = np.array(self.csvrows)



        '''if self.raw_data is None:
            raise ValueError('tried to build model with nothing loaded')
        else:
            session_names, sessionindices = np.unique(self.raw_data[:,4],return_index=True)
            print(session_names)
            for s_ix in range(len(session_names)):
                if s_ix == len(session_names)-1:
                    sdat = self.raw_data[sessionindices[s_ix]:,:]
                else:
                    sdat = self.raw_data[sessionindices[s_ix]:sessionindices[s_ix+1],:]
                trialnums, trialindices = np.unique(sdat[:,6],return_index=True)
                print(trialnums)
                for t_ix in range(len(trialnums)):
                    trial = Trial(session_names[s_ix])
                    if t_ix == len(trialnums) - 1:
                        temp_tdat = sdat[trialindices[t_ix]:,:]

                    else:
                        temp_tdat = sdat[trialindices[t_ix]:trialindices[t_ix + 1],:]

                    bldat = temp_tdat[temp_tdat[:, 5] < 0,:]  # baseline
                    tdat = temp_tdat[temp_tdat[:, 5] >= 0,:]  # data

                    #bldat = np.array([[0,0,0,0]])
                    #tdat = temp_tdat

                    if np.isnan(np.mean(bldat[:,0])):
                        print('this one was nan')
                        continue

                    trial.t = tdat[:,5]
                    trial.t_zyg = tdat[:,0] - np.mean(bldat[:,0])
                    trial.t_cor = tdat[:,1] - np.mean(bldat[:,1])
                    trial.r_zyg = tdat[:,2] - np.mean(bldat[:,2])
                    trial.r_cor = tdat[:,3] - np.mean(bldat[:,3])
                    trial.label = tdat[0,8]
                    trial.number = tdat[0,6]
                    TR = trial.sessionID.split('_')
                    T = 'P'+TR[0].replace('T','')
                    R = 'P'+TR[1].replace('R','')
                    csvrow = np.where((self.comcsv[:,0] == T) & (self.comcsv[:,1]==R) & (self.comcsv[:,3]==trial.number))[0]
                    print(csvrow)
                    trial.response = self.comcsv[csvrow,5]
                    trial.correct = self.comcsv[csvrow,7]
                    trial.relationship = self.comcsv[csvrow,10]

                    self.trials.append(trial)
                    if not trial.label in self.gest2trial.keys():
                        self.gest2trial[trial.label] = [trial]
                    else: self.gest2trial[trial.label].append(trial)
                    self.features.append(self.get_features(trial))
                    self.labels.append(trial.label)
                    self.response_labels.append(trial.response)
                    self.correct.append(trial.correct)
                    self.relationships.append(trial.relationship)

            self.features = np.array(self.features,ndmin=2)'''

    def get_features(self,trial):
        features = []
        ix = 0
        cols = ['-r','-g','-b','-k']
        for femg in [trial.t_zyg,trial.t_cor,trial.r_zyg,trial.r_cor]:
            if femg is None:
                features.extend([np.nan,np.nan,np.nan,np.nan,np.nan,np.nan])
                continue
            V = femg.values
            #V = self.smooth(femg,128*2+1)
            #V = V[(trial.t>5) & (trial.t<10)]
            mean_raw = np.mean(V)
            std_raw = np.std(V)
            #print(mean_raw,std_raw)
            mean_abs_fdif = np.mean(np.abs(np.diff(V)))
            mean_abs_nfdif = mean_abs_fdif/std_raw
            sdif = V[2:]-V[:-2]
            mean_abs_sdif = np.mean(np.abs(sdif))
            mean_abs_nsdif = mean_abs_sdif/std_raw
            features.extend([mean_raw,std_raw,mean_abs_fdif,mean_abs_nfdif,mean_abs_sdif,mean_abs_nsdif])
            ix += 1
        print(features)
        #plt.show()
        return features


    def smooth(self,a, WSZ):
        # a: NumPy 1-D array containing the data to be smoothed
        # WSZ: smoothing window size needs, which must be odd number,
        # as in the original MATLAB implementation
        out0 = np.convolve(a, np.ones(WSZ, dtype=int), 'valid') / WSZ
        r = np.arange(1, WSZ - 1, 2)
        start = np.cumsum(a[:WSZ - 1])[::2] / r
        stop = (np.cumsum(a[:-WSZ:-1])[::2] / r)[::-1]
        return np.concatenate((start, out0, stop))

    def save_features(self,outfile):
        pickle.dump((self.features,self.labels,self.response_labels,self.correct,self.relationships,self.csvrows),open(outfile,'wb'))

    def plot_gesture(self,gest):
        tr = self.gest2trial[gest][0]
        plt.plot(tr.t,self.smooth(tr.t_zyg,128*2+1),'-r')
        plt.show()

class Trial:
    def __init__(self,sID):
        self.sessionID = sID
        self.t = []
        self.t_zyg = []
        self.t_cor = []
        self.r_zyg = []
        self.r_cor = []
        self.label = None
        self.number = None
        self.response = None
        self.correct = None
        self.relationship = None
        self.csvrow = None

m = Model()
#m.load_text('expt2-femg-data-raw.csv','expt2-communication-data-all.csv')
m.load_text(['fEMG_clean_rcor.csv','fEMG_clean_rzyg.csv','fEMG_clean_tcor.csv','fEMG_clean_tzyg.csv'],['r_cor','r_zyg','t_cor','t_zyg'],'expt1_communication_data.csv')
#m.build_model('sdat.pkl')
m.build_model()
m.save_features('out_clean_1s_include_rows.pkl')
#m.plot_gesture('calming')
#m.save_trials('out.pkl')