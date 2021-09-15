from sklearn.ensemble import RandomForestClassifier, GradientBoostingClassifier
import cPickle as pickle
import numpy as np
import matplotlib.pyplot as plt
import matplotlib
import pandas as pd
import random
from sklearn import model_selection
import sklearn.metrics
import copy
import os

class ModelFactory:
    def __init__(self,features,cued_labels,responded_labels,correct,relationships,training_proportion=.8,csvrows=None):
        self.features = features
        self.cued_labels = cued_labels
        self.responded_labels = responded_labels
        self.correct = correct
        self.relationships = relationships
        self.training_proportion = training_proportion
        self.csvrows = csvrows

    def get_model(self,selected_feats,selected_obs,selected_lbls,title='',csvrows = None):
        features = None
        if selected_feats == Model.TOUCHER_AND_RECEIVER:
            features = self.features
        elif selected_feats == Model.TOUCHER:
            features = self.features[:,:12]
        elif selected_feats == Model.RECEIVER:
            features = self.features[:,12:]

        if self.csvrows is None:
            original_ixs = np.array(range(features.shape[0]))
        else:
            original_ixs = copy.deepcopy(self.csvrows)

        labels = None
        if selected_lbls == Model.CUED:
            labels = copy.deepcopy(self.cued_labels)
        elif selected_lbls == Model.RESPONDED:
            labels = copy.deepcopy(self.responded_labels)

        correct = copy.deepcopy(self.correct)
        relationships = copy.deepcopy(self.relationships)

        if Model.ALL in selected_obs:
            pass
        if Model.CORRECT in selected_obs:
            ix = np.where(correct == 1L)[0]
            features = features[ix,:]
            labels = labels[ix]
            relationships = relationships[ix]
            correct = correct[ix]
            original_ixs = original_ixs[ix]
        if Model.INCORRECT in selected_obs:
            ix = np.where(correct == 0L)[0]
            features = features[ix, :]
            labels = labels[ix]
            relationships = relationships[ix]
            correct = correct[ix]
            original_ixs = original_ixs[ix]
        if Model.ROMANTIC in selected_obs:
            ix = np.where(relationships == 'romantic')[0]
            features = features[ix, :]
            labels = labels[ix]
            relationships = relationships[ix]
            correct = correct[ix]
            original_ixs = original_ixs[ix]
        if  Model.FRIENDSHIP in selected_obs:
            ix = np.where(relationships == 'friendship')[0]
            features = features[ix, :]
            labels = labels[ix]
            relationships = relationships[ix]
            correct = correct[ix]
            original_ixs = original_ixs[ix]

        flag = features.shape[0] == len(labels) == len(correct) == len(relationships)
        if not flag:
            print('ERROR HERE')

        labels = labels[~np.isnan(features).any(axis=1)]
        original_ixs = original_ixs[~np.isnan(features).any(axis=1)]
        features = features[~np.isnan(features).any(axis=1)]

        return Model(features,labels,title,self.training_proportion,original_ixs)

class Model:
    #features to include
    TOUCHER_AND_RECEIVER = 0
    TOUCHER = 1
    RECEIVER = 2
    feats = [TOUCHER_AND_RECEIVER,TOUCHER,RECEIVER]

    #observations to include
    ALL = 3
    CORRECT = 4
    INCORRECT = 5
    ROMANTIC = 6
    FRIENDSHIP = 7
    obs = [ALL,CORRECT,INCORRECT,ROMANTIC,FRIENDSHIP]

    #labels to use
    CUED = 8
    RESPONDED = 9
    lbls = [CUED,RESPONDED]

    def __init__(self,features,labels,title,train_proportion,original_ixs):

        self.original_ixs = copy.deepcopy(original_ixs)
        self.features = features
        self.labels = copy.deepcopy(labels)
        self.rf = RandomForestClassifier(n_estimators=1000,random_state = 0)

        self.training_proportion = train_proportion

        self.accuracy = None
        self.std_accuracy = None
        self.confusion_matrix = None
        self._cms = None
        self._accuracies = None
        self.title = title
        self.n_classed = None

    def run(self,LBLS_num):
        # rpts = 5
        # accuracies = []
        # self.confusion_matrix = np.zeros((len(LBLS_num), len(LBLS_num)))
        # for i0 in range(rpts):
        #     ntrain = int(self.features.shape[0]*self.training_proportion)
        #     shufix = range(self.features.shape[0])
        #     random.shuffle(shufix)
        #     features = self.features[shufix, :]
        #     #print(self.labels)
        #     #print(shufix)
        #     labels = self.labels[shufix]
        #
        #     train_features = features[:ntrain, :]
        #     train_labels = labels[:ntrain]
        #
        #     test_features = features[ntrain:, :]
        #     test_labels = labels[ntrain:]
        #
        #     self.rf.fit(train_features, train_labels)
        #     predictions = self.rf.predict(test_features)
        #
        #     accuracies.append(np.sum(predictions==test_labels)/float(len(predictions)))
        #     for ri in range(len(LBLS_num)):
        #         for ci in range(len(LBLS_num)):
        #             idxs = np.where((test_labels == LBLS_num[ri]) & (predictions == LBLS_num[ci]))
        #             denom = float(np.sum(test_labels == LBLS_num[ri]))
        #             if denom == 0:
        #                 self.confusion_matrix[ri,ci] += 0
        #             else:
        #                 self.confusion_matrix[ri, ci] += 100. * len(idxs[0]) / denom
        # print(accuracies)
        #
        # self.accuracy= np.mean(accuracies)
        # self.std_accuracy = np.std(accuracies)
        # self.confusion_matrix /= float(rpts)

        NFOLD = 10
        self._accuracies = []
        self.n_classed = np.zeros((len(LBLS_num), len(LBLS_num)))
        self._cms = []

        self.correct_ixs = []
        self.incorrect_ixs = []
        X = self.features
        y = self.labels
        kf = model_selection.KFold(n_splits=NFOLD)
        self.confusion_matrix = np.zeros((len(LBLS_num), len(LBLS_num)))
        for train_index, test_index in kf.split(X):
            X_train, X_test = X[train_index], X[test_index]
            y_train, y_test = y[train_index], y[test_index]

            self.rf.fit(X_train, y_train)
            predictions = self.rf.predict(X_test)
            self._accuracies.append(np.sum(predictions == y_test) / float(len(predictions)))


            self.correct_ixs.extend(self.original_ixs[test_index[predictions==y_test]])
            self.incorrect_ixs.extend(self.original_ixs[test_index[predictions != y_test]])

            #_cm = np.zeros((len(LBLS_num), len(LBLS_num)))
            for ri in range(len(LBLS_num)):
                for ci in range(len(LBLS_num)):
                    idxs = np.where((y_test == LBLS_num[ri]) & (predictions == LBLS_num[ci]))
                    self.n_classed[ci] += len(idxs[0])
                    self.confusion_matrix[ri,ci] += len(idxs[0])
                    # denom = float(np.sum(y_test == LBLS_num[ri]))
                    # if denom == 0:
                    #     print('this is happening.')
                    #     self.confusion_matrix[ri,ci] += 0
                    # else:
                    #     self.confusion_matrix[ri, ci] += 100. * len(idxs[0]) / denom
                    #     _cm[ri,ci] = len(idxs[0])/denom
            #self._cms.append(_cm)
        self.accuracy = np.mean(self._accuracies)
        self.std_accuracy = np.std(self._accuracies)
        for ri in range(self.confusion_matrix.shape[0]):
            self.confusion_matrix[ri,:] /= np.sum(self.confusion_matrix[ri,:])/100.
        #self.confusion_matrix /= float(NFOLD)
        print(self._accuracies)

        # y_pred = cross_val_predict(self.rf,self.features,self.labels,cv=10)
        # self.confusion_matrix = sklearn.metrics.confusion_matrix(self.labels,y_pred)
        # self.accuracy = sklearn.metrics.accuracy_score(self.labels,y_pred)

if __name__ == '__main__':

    #TODO I'm missing some data
    # all_features,all_labels,all_response_labels,all_correct,all_relationships = pickle.load(open('out_clean_1s.pkl','rb'))
    all_features, all_labels, all_response_labels, all_correct, all_relationships,all_csvrows = pickle.load(open('out_clean_1s_include_rows.pkl', 'rb'))
    all_csvrows = np.array(all_csvrows).flatten()


    # #to turn on shuffling
    # shufix = range(len(all_labels))
    # random.shuffle(shufix)
    # all_features = all_features[shufix,:]
    # all_labels = np.array(all_labels)[shufix]
    # all_response_labels = np.array(all_response_labels)[shufix]
    # all_correct = np.array(all_correct)[shufix]
    # all_relationships = np.array(all_relationships)[shufix]
    # all_csvrows = np.array(all_csvrows)[shufix]


    LBLS = np.unique(all_labels).tolist()
    LBLS.extend(['open','other']) #idk if this is the right call
    LBLS_num = np.array(range(len(LBLS)))
    all_labels = np.array([LBLS.index(l) for l in all_labels],dtype=np.int8)
    #print(all_response_labels)
    all_response_labels= np.array([LBLS.index(l) for l in all_response_labels],dtype=np.int8)
    all_correct = np.array([int(c[0]) for c in all_correct])
    all_relationships = np.array([s[0] for s in all_relationships],dtype=np.string_)
    #ntrain = int(float(len(all_labels))*.8)


    #mf = ModelFactory(all_features, all_labels, all_response_labels, all_correct, all_relationships)
    mf = ModelFactory(all_features, all_labels, all_response_labels, all_correct, all_relationships, csvrows=all_csvrows)

    '''modelparams1 = [(Model.TOUCHER_AND_RECEIVER,[Model.ALL],Model.CUED,'T+R (Cued)'),
              (Model.TOUCHER,[Model.ALL],Model.CUED,'Toucher (Cued)'),
              (Model.RECEIVER,[Model.ALL],Model.CUED,'Receiver (Cued)'),
              (Model.TOUCHER_AND_RECEIVER, [Model.ALL], Model.RESPONDED,'T+R (Responded)'),
              (Model.TOUCHER, [Model.ALL], Model.RESPONDED,'Toucher (Responded)'),
              (Model.RECEIVER, [Model.ALL], Model.RESPONDED,'Receiver (Responded)'),
              (Model.TOUCHER_AND_RECEIVER,[ Model.CORRECT], Model.CUED,'T+R (Correct)'),
              (Model.TOUCHER, [Model.CORRECT], Model.CUED,'Toucher (Correct)'),
              (Model.RECEIVER, [Model.CORRECT], Model.CUED,'Receiver (Correct)')]
    modelparams2 = [(Model.TOUCHER_AND_RECEIVER, [Model.ROMANTIC], Model.CUED,'T+R (Romantic)'),
              (Model.TOUCHER, [Model.ROMANTIC], Model.CUED,'Toucher (Romantic)'),
              (Model.RECEIVER, [Model.ROMANTIC], Model.CUED,'Receiver (Romantic)'),
              (Model.TOUCHER_AND_RECEIVER, [Model.FRIENDSHIP], Model.CUED,'T+R (Friends)'),
              (Model.TOUCHER, [Model.FRIENDSHIP], Model.CUED,'Toucher (Friends)'),
              (Model.RECEIVER, [Model.FRIENDSHIP], Model.CUED,'Receiver (Friends)'),
              (Model.TOUCHER_AND_RECEIVER, [Model.ROMANTIC,Model.CORRECT], Model.CUED,'T+R (Romantic, Correct)'),
              (Model.TOUCHER, [Model.ROMANTIC,Model.CORRECT], Model.CUED,'Toucher (Romantic, Correct)'),
              (Model.RECEIVER, [Model.ROMANTIC,Model.CORRECT], Model.CUED,'Receiver (Romantic, Correct)')]'''

    # modelparams1 = [(Model.TOUCHER, [Model.ROMANTIC,Model.CORRECT], Model.CUED,'Toucher (Romantic, Correct)'),
    #                 (Model.RECEIVER, [Model.ROMANTIC,Model.CORRECT], Model.CUED,'Receiver (Romantic, Correct)'),
    #                 (Model.TOUCHER, [Model.ROMANTIC, Model.INCORRECT], Model.CUED, 'Toucher (Romantic, Incorrect)'),
    #                 (Model.RECEIVER, [Model.ROMANTIC, Model.INCORRECT], Model.CUED, 'Receiver (Romantic, Incorrect)')
    #                 ]
    # modelparams2 = [(Model.TOUCHER, [Model.FRIENDSHIP,Model.CORRECT], Model.CUED,'Toucher (Friendship, Correct)'),
    #                 (Model.RECEIVER, [Model.FRIENDSHIP,Model.CORRECT], Model.CUED,'Receiver (Friendship, Correct)'),
    #                 (Model.TOUCHER, [Model.FRIENDSHIP, Model.INCORRECT], Model.CUED, 'Toucher (Friendship, Incorrect)'),
    #                 (Model.RECEIVER, [Model.FRIENDSHIP, Model.INCORRECT], Model.CUED, 'Receiver (Friendship, Incorrect)')
    #                 ]
    #
    # modelparams3 = [(Model.RECEIVER, [Model.ALL],Model.RESPONDED,'Responded (Both)'),
    #                 (Model.RECEIVER, [Model.FRIENDSHIP], Model.RESPONDED, 'Responded (Friends)'),
    #                 (Model.RECEIVER, [Model.ROMANTIC], Model.RESPONDED, 'Responded (Romantic)')]

    # modelparams4 = [(Model.TOUCHER, [Model.ALL,Model.CORRECT],Model.CUED,'Toucher'),
    #                 (Model.RECEIVER, [Model.ALL,Model.CORRECT], Model.CUED, 'Receiver'),]

    modelparams5 = [(Model.TOUCHER, [Model.ALL,Model.ALL],Model.CUED,'Toucher'),
                    (Model.RECEIVER, [Model.ALL,Model.ALL], Model.CUED, 'Receiver'),]
    #modelparamsets = [modelparams1,modelparams2]

    modelparamsets = [modelparams4]
    modelsets = []
    all_models = []
    for paramset in modelparamsets:
        modelset = []
        for params in paramset:
            model = mf.get_model(*params)
            model.run(LBLS_num)
            modelset.append(model)
            all_models.append(model)
        modelsets.append(modelset)

    for modelset in modelsets:
        fig,ax = plt.subplots(2,2,figsize=(4,2))
        ax = ax.flatten()

        for i1 in range(len(modelset)):
            model = modelset[i1]
            im=ax[i1].imshow(np.flipud(model.confusion_matrix),cmap='Greys')
            ax[i1].set_yticks(range(len(LBLS)))
            ax[i1].set_yticklabels(LBLS[::-1])
            ax[i1].set_xticks(range(len(LBLS)))
            ax[i1].set_xticklabels(LBLS,rotation=90)
            plt.subplot(ax[i1])
            #cax=plt.gca() #get the current axes
            #PCM=cax.get_children()[2]
            #plt.colorbar(PCM,ax=cax)
            fig.colorbar(im)
            im.set_clim(0,35)
            plt.title(model.title+' ('+str(int(model.accuracy*100)) + '%)',fontsize=12)
        font = {'family' : 'normal',
                'size'   : 12}
        matplotlib.rc('font', **font)
        plt.subplots_adjust(top=0.935,
                            bottom=0.135,
                            left=0.1,
                            right=0.95,
                            hspace=1.0,
                            wspace=0.545)
    #plt.tight_layout()
    accuracy = [m.accuracy for m in all_models]
    stdevs = [m.std_accuracy for m in all_models]
    titles = [m.title for m in all_models]
    idx = np.argsort(accuracy)
    accuracy = [accuracy[i] for i in idx]
    titles = [titles[i] for i in idx]
    stdevs = [stdevs[i] for i in idx]

    figb,axb = plt.subplots()
    for i0 in range(len(accuracy)):
        plt.bar(i0,accuracy[i0],color=(.4,.4,.4))
        plt.errorbar([i0,i0],[accuracy[i0],accuracy[i0]],stdevs[i0],ecolor=(0,0,0),capsize=10,barsabove=True)
    axb.set_xticks(range(len(accuracy)))
    axb.set_xticklabels(titles,rotation=45,ha='right')
    axb.set_ylabel('Classification accuracy')
    font = {'family' : 'normal',
                'size'   : 12}
    matplotlib.rc('font', **font)
    plt.subplots_adjust(top=0.95,
                        bottom=0.505,
                        left=0.2,
                        right=0.95,
                        hspace=0.2,
                        wspace=0.2)
    for model in all_models:
        print model.title
        print(np.array(model.correct_ixs)+2,np.array(model.incorrect_ixs)+2)
    pickle.dump(all_models,open('MLmodels_3_7_2019.pkl','wb'))  #hasnt been run yet, ML models final SVM is the most recent


    #plt.savefig('testfig.png',dpi=600, bbox_inches = "tight")
    plt.show()
