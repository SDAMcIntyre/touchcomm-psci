import pickle
from ML import Model
import numpy as np
import scipy.stats
import matplotlib
import matplotlib.pyplot as plt
from matplotlib import lines
import matplotlib.colors as mcolors

def make_colormap(seq):
    """Return a LinearSegmentedColormap
    seq: a sequence of floats and RGB-tuples. The floats should be increasing
    and in the interval (0,1).
    """
    seq = [(None,) * 3, 0.0] + list(seq) + [1.0, (None,) * 3]
    cdict = {'red': [], 'green': [], 'blue': []}
    for i, item in enumerate(seq):
        if isinstance(item, float):
            r1, g1, b1 = seq[i - 1]
            r2, g2, b2 = seq[i + 1]
            cdict['red'].append([item, r1, r2])
            cdict['green'].append([item, g1, g2])
            cdict['blue'].append([item, b1, b2])
    return mcolors.LinearSegmentedColormap('CustomMap', cdict)

def plot_cmat(fig,ax,LBLS,model,rvb):
    print(model.title)
    print(str(model.confusion_matrix[:-2,:-2]).replace('[','').replace(']',''))
    print(LBLS)
    im=ax.imshow(np.flipud(model.confusion_matrix[:-2,:-2]),cmap=rvb)
    ax.set_yticks(range(len(LBLS)))
    ax.set_yticklabels(LBLS[::-1])
    ax.set_xticks(range(len(LBLS)))
    ax.set_xticklabels(LBLS,rotation=90)
    plt.subplot(ax)
    #cax=plt.gca() #get the current axes
    #PCM=cax.get_children()[2]
    #plt.colorbar(PCM,ax=cax)
    cbar = fig.colorbar(im)
    im.set_clim(10,35)
    im.axes.tick_params(color=(.4,.4,.4), labelcolor=(.4,.4,.4))
    plt.title(model.title,fontsize=12,color=(.4,.4,.4))
    [ax.spines[k].set_color((.4, .4, .4)) for k in ax.spines.keys()]
    [t.set_color((.4, .4, .4)) for t in ax.xaxis.get_ticklines()]
    [t.set_color((.4, .4, .4)) for t in ax.xaxis.get_ticklabels()]
    [t.set_color((.4, .4, .4)) for t in ax.yaxis.get_ticklines()]
    [t.set_color((.4, .4, .4)) for t in ax.yaxis.get_ticklabels()]
    ax.yaxis.label.set_color((.4, .4, .4))
    ax.xaxis.label.set_color((.4, .4, .4))

    fg_color = (.4,.4,.4)
    # set colorbar label plus label color
    plt.setp(plt.getp(cbar.ax.axes, 'yticklabels'), color=fg_color)


# models = pickle.load(open('MLmodels_final.pkl','rb')) #from 8-21-2018
models = pickle.load(open('MLmodels_3_7_2019.pkl','rb'))

all_features,all_labels,all_response_labels,all_correct,all_relationships = pickle.load(open('out_all.pkl','rb'))
LBLS = np.unique(all_labels).tolist()
#LBLS.extend(['open','other']) #idk if this is the right call
LBLS_num = np.array(range(len(LBLS)))


titles = [m.title for m in models]
touchertitles = ['Toucher',]
receivertitles = ['Receiver',]
# romantic = []
# friendship = []
# for title in titles:
#     if 'Romantic' in title:
#         romantic.append(models[titles.index(title)])
#     else:
#         friendship.append(models[titles.index(title)])
#
# accuracyR = [m.accuracy for m in romantic]
# accuracyF = [m.accuracy for m in friendship]
# idxR = np.argsort(accuracyR)[::-1]
# idxF = np.argsort(accuracyF)[::-1]
# models = [romantic[i] for i in idxR]
# models.extend([friendship[i] for i in idxF])

idxT = [titles.index(t) for t in touchertitles]
idxR = [titles.index(t) for t in receivertitles]
modelsT = [models[i] for i in idxT]
modelsR = [models[i] for i in idxR]
#models = [models[i] for i in idx]


colorR = np.array([141,160,203])/255.
colorT = np.array([252,141,98])/255.

fig = plt.figure()
axb = plt.subplot2grid((2,6),(0,4),colspan=1)
ix = 0
#for i0 in [0,1,2.5,3.5]:
for i0 in [.5,]:
    eb_lw = .4
    W = .8
    model = modelsT[ix]
    OS = -.5
    l1=plt.bar(i0+OS,model.accuracy,color=colorT,width=W)
    plt.errorbar([i0+OS,i0+OS],[model.accuracy,model.accuracy],model.std_accuracy,ecolor=(0,0,0),capsize=5,barsabove=True,lw=eb_lw,markeredgewidth=eb_lw)
    tot = float(len(model.labels))
    dat = np.vstack(([tot * model.accuracy, tot * (1 - model.accuracy)], [tot * (1. / 6.), tot * (1. - 1. / 6.)]))
    print('dat=',dat)
    p= scipy.stats.chi2_contingency(dat)[1]
    #p=.05
    sigtxt = ''
    if p <= .001:
        sigtxt = '***'
    elif p <= .01:
        sigtxt = '**'
    elif p <= .05:
        sigtxt = '*'
    plt.text(i0+OS,model.accuracy+model.std_accuracy+.02,sigtxt,ha='center',va='bottom',color=(0,0,0))
    print('toucher',model.accuracy,model.std_accuracy)

    model = modelsR[ix]
    OS = .5
    l2=plt.bar(i0 +OS, model.accuracy, color=colorR,width=W)
    plt.errorbar([i0+OS, i0+OS], [model.accuracy, model.accuracy], model.std_accuracy, ecolor=(0,0,0), capsize=5,
                 barsabove=True,lw=eb_lw,markeredgewidth=eb_lw)
    tot = float(len(model.labels))
    dat = np.vstack(([tot * model.accuracy, tot * (1 - model.accuracy)], [tot * (1. / 6.), tot * (1. - 1. / 6.)]))
    print('dat2=',dat)
    p= scipy.stats.chi2_contingency(dat)[1]
    #p = .05
    sigtxt = ''
    if p <= .001:
        sigtxt = '***'
    elif p <= .01:
        sigtxt = '**'
    elif p <= .05:
        sigtxt = '*'
    plt.text(i0+OS, model.accuracy + model.std_accuracy + .02, sigtxt, ha='center', va='bottom',color=(0,0,0))
    print('receiver', model.accuracy, model.std_accuracy)
    ix += 1


# leg=axb.legend((l1[0],l2[0]),['Toucher','Receiver'],frameon=False)
# for text in leg.get_texts():
#     text.set_color((.4,.4,.4))

# plt.text(.5,-.2,'Romantic',ha='center',va='top',color=(.4,.4,.4))
# plt.text(3,-.2,'Friendship',ha='center',va='top',color=(.4,.4,.4))
#
# LW = .5
# line = lines.Line2D([-.5,-.5],[-.1,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)
# line = lines.Line2D([-.5,1.5],[-.15,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)
# line = lines.Line2D([1.5,1.5],[-.1,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)
#
# line = lines.Line2D([2,2],[-.1,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)
# line = lines.Line2D([2,4],[-.15,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)
# line = lines.Line2D([4,4],[-.1,-.15],color=(.4,.4,.4),lw=LW)
# line.set_clip_on(False)
# axb.add_line(line)


#axb.set_xticks([0,1,2.5,3.5])
axb.set_xticks([0,1])
axb.set_xticklabels(['Toucher','Receiver'],rotation=0,ha='center')
axb.set_ylabel('Classification accuracy')
axb.spines['top'].set_visible(False)
axb.spines['right'].set_visible(False)
axb.spines['left'].set_color((.4,.4,.4))
axb.spines['bottom'].set_color((.4,.4,.4))
axb.yaxis.set_ticks_position('left')
axb.xaxis.set_ticks_position('bottom')
plt.plot([-.5,4],[1./6.,1./6.],color=(0,0,0),lw=eb_lw,dashes=[25, 15])
axb.set_ylim(0,.6)
axb.set_xlim(-.5,1.5)
[t.set_color((.4,.4,.4)) for t in axb.xaxis.get_ticklines()]
[t.set_color((.4,.4,.4)) for t in axb.xaxis.get_ticklabels()]
[t.set_color((.4,.4,.4)) for t in axb.yaxis.get_ticklines()]
[t.set_color((.4,.4,.4)) for t in axb.yaxis.get_ticklabels()]
axb.yaxis.label.set_color((.4,.4,.4))

# font = {'family' : 'arial',
#             'size'   : 12}
# matplotlib.rc('font', **font)

# plt.subplots_adjust(top=0.95,
#                     bottom=0.505,
#                     left=0.2,
#                     right=0.95,
#                     hspace=0.2,
#                     wspace=0.2)

c = mcolors.ColorConverter().to_rgb
N=1000

cmapT = make_colormap(
    [c('white'), colorT,.99, colorT])
cmapR = make_colormap(
    [c('white'), colorR,.99, colorR])
colors = np.random.uniform(0, 35, size=(N,))


RomTC = models[titles.index(touchertitles[0])]
#RomTIC = models[titles.index(touchertitles[1])]
RomRC = models[titles.index(receivertitles[0])]
#RomRIC = models[titles.index(receivertitles[1])]
plot_cmat(fig,plt.subplot2grid((2,6),(0,0),colspan=2),LBLS,RomTC,cmapT)
#plot_cmat(fig,plt.subplot2grid((3,2),(1,0)),LBLS,RomTIC,cmapT)
plot_cmat(fig,plt.subplot2grid((2,6),(0,2),colspan=2),LBLS,RomRC,cmapR)
#plot_cmat(fig,plt.subplot2grid((3,2),(1,1)),LBLS,RomRIC,cmapR)

print('HERE',RomTC.features)

plt.show()
