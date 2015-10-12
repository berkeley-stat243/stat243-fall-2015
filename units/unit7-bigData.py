#####################################################
# 6: Hadoop, MapReduce, and Spark
#####################################################

### 6.3 Spark

### 6.3.1 Getting set up


## @knitr spark-setup

export SPARK_VERSION=1.5.1
export CLUSTER_SIZE=12  # number of slave nodes
export mycluster=sparkvm-paciorek # need unique name relative to other users

# I unzipped the Spark tarball to /usr/lib/spark via sudo on BCE
cd /usr/lib/spark/ec2

# set Amazon secret keys (manually or in my case by querying them elsewhere)
#AWS_ACCESS_KEY_ID=blah
AWS_ACCESS_KEY_ID=$(grep -i "^AWS_ACCESS_KEY_ID" ~/stat243-fall-2015-credentials.boto | cut -d' ' -f3)
#AWS_SECRET_ACCESS_KEY=blahhhh
AWS_SECRET_ACCESS_KEY=$(grep -i "^AWS_SECRET_ACCESS_KEY" ~/stat243-fall-2015-credentials.boto | cut -d' ' -f3)

### DO NOT HARD CODE YOUR AMAZON SECRET KEY INFORMATION INTO ANY PUBLIC FILE, INCLUDING A GITHUB REPO !!!!! ###

# start cluster
./spark-ec2 -k chris_paciorek@yahoo.com:stat243-fall-2015 -i ~/.ssh/stat243-fall-2015-ssh_key.pem  \
 --region=us-west-2 -s ${CLUSTER_SIZE} -v ${SPARK_VERSION} launch ${mycluster}

# login to cluster
# as root
./spark-ec2 -k ec2star -i ~/.ssh/stat243-fall-2015-ssh_key.pem --region=us-west-2 \
   login ${mycluster}
# or you can ssh in directly if you know the URL
# ssh -i ~/.ssh/stat243-fall-2015-ssh_key.pem root@ec2-54-71-204-234.us-west-2.compute.amazonaws.com


# you can check your nodes via the EC2 management console

# to logon to one of the slaves, look at /root/ephemeral-hdfs/conf/slaves
# and ssh to that address
ssh `head -n 1 /root/ephemeral-hdfs/conf/slaves`

# We can view system status through a web browser interface

# on master node of the EC2 cluster, do:
MASTER_IP=`cat /root/ephemeral-hdfs/conf/masters`
echo ${MASTER_IP}
# Point a browser on your own machine to the result of the next command
# you'll see info about the "Spark Master", i.e., the cluster overall
echo "http://${MASTER_IP}:8080/"
# Point a browser on your own machine to the result of the next command
# you'll see info about the "Spark Stages", i.e., the status of Spark tasks
echo "http://${MASTER_IP}:4040/"
# Point a browser on your own machine to the result of the next command
# you'll see info about the HDFS"
echo "http://${MASTER_IP}:50070/"

# when you are done and want to shutdown the cluster:
#  IMPORTANT to avoid extra charges!!!
./spark-ec2 --region=us-west-2 destroy ${mycluster}

## @knitr spark-hdfs


export PATH=$PATH:/root/ephemeral-hdfs/bin/

hadoop fs -mkdir /data
hadoop fs -mkdir /data/airline

df -h
mkdir /mnt/airline
scp paciorek@smeagol.berkeley.edu:/scratch/users/paciorek/243/AirlineData/[12]*bz2 \ 
   /mnt/airline
# for in-class demo:
# scp paciorek@smeagol.berkeley.edu:/scratch/users/paciorek/243/AirlineData/198*bz2 /mnt/airline

hadoop fs -copyFromLocal /mnt/airline/*bz2 /data/airline

# check files on the HDFS, e.g.:
hadoop fs -ls /data/airline

# get numpy installed
# there is a glitch in the EC2 setup that Spark provides -- numpy is not installed on the version of Python that Spark uses (Python 2.7). To install numpy on both the master and worker nodes, do the following as root on the master node.
yum install python27-pip python27-devel
pip-2.7 install 'numpy==1.9.2'  # 1.10.1 has an issue with a warning in median()
/root/spark-ec2/copy-dir /usr/local/lib64/python2.7/site-packages/numpy

# pyspark is in /root/spark/bin
export PATH=${PATH}:/root/spark/bin
# start Spark's Python interface as interactive session
pyspark

## @knitr spark-data

from operator import add
import numpy as np

lines = sc.textFile('/data/airline').cache()
numLines = lines.count()

# particularly for in-class demo - good to repartition the 3 files to more partitions
# lines = lines.repartition(96).cache()

# mapper
def stratify(line):
    vals = line.split(',')
    return(vals[16], 1)

result = lines.map(stratify).reduceByKey(add).collect()
# reducer is simply the addition function

# >>> result
#[(u'Origin', 22), (u'CIC', 7281), (u'LAN', 67897), (u'LNY', 289), (u'DAB', 86656), (u'APF', 4074), (u'ATL', 6100953), (u'BIL', 92503), (u'JAN', 190044), (u'GTR', 7520), (u'ISO', 5945), (u'SEA', 1984077), (u'PIT', 2072303), (u'ONT', 774679), (u'ROW', 1196), (u'PWM', 161602), (u'FAY', 44564), (u'SAN', 1546835), (u'ADK', 589), (u'ADQ', 10327), (u'IAD', 1336957), (u'ANI', 475), (u'CHO', 19324), (u'HRL', 116018), (u'ACV', 23782), (u'DAY', 380459), (u'ROA', 69361), (u'VIS', 1993), (u'PSC', 38408), (u'MDW', 1170344), (u'MRY', 67926), (u'MCO', 1967493), (u'EKO', 12808), (u'RNO', 510023), (u'TPA', 1321652), (u'OME', 21403), (u'DAL', 952216), (u'GJT', 34921), (u'ALB', 292764), (u'SJT', 16590), (u'CAK', 80821), (u'TUP', 1971), (u'MKG', 396), (u'DEN', 3319905), (u'MDT', 167293), (u'RKS', 954), (u'GSP', 200147), (u'LAW', 18019), (u'MCN', 7203), (u'PIA', 44780), (u'ROC', 368099), (u'BQK', 6934), (u'MSP', 2754997), (u'ACT', 21081), (u'SBA', 119959), (u'HPN', 125500), (u'RFD', 1560), (u'CCR', 4465), (u'BWI', 1717380), (u'SJU', 461019), (u'SAV', 185855), (u'HOU', 1205951), (u'BPT', 8452), (u'RDU', 103678 ....

# this counting by key could have been done
# more easily using countByKey()

vals = [x[1] for x in result]
sum(vals) == numLines  # a bit of a check
# True
[x[1] for x in result if x[0] == "SFO"]  # SFO result
# [2733910]

# if don't collect, can grab a few results
output = lines.map(stratify).reduceByKey(add)
output.take(5)
#[(u'Origin', 22), (u'CIC', 7281), (u'LAN', 67897), (u'LNY', 289), (u'DAB', 86656)]

# also, you can have interim results stored as objects
mapped = lines.map(stratify)
result = mapped.reduceByKey(add).collect()


lines.filter(lambda line: "SFO" in line.split(',')[16]).saveAsTextFile('/data/airline-SFO')

## make sure it's all in one chunk for easier manipulation on master
lines.filter(lambda line: "SFO" in line.split(',')[16]).repartition(1).saveAsTextFile('/data/airline-SFO2')
#lines.filter(lambda line: "SFO" in line.split(',')[16]).repartition(1).
#saveAsTextFile('/data/airline-SFO2')

## @knitr spark-nonstandard

def computeKeyValue(line):
    vals = line.split(',')
    # key is carrier-month-origin-destination
    keyVals = '-'.join([vals[x] for x in [8,1,16,17]])
    if vals[0] == 'Year':
        return('0', [0,0,1,1])
    cnt1 = 1
    cnt2 = 1
    # 14 and 15 are arrival and departure delays
    if vals[14] == 'NA':
        vals[14] = '0'
        cnt1 = 0
    if vals[15] == 'NA':
        vals[15] = '0'
        cnt2 = 0
    return(keyVals, [int(vals[14]), int(vals[15]), cnt1, cnt2])


def medianFun(input):
    if len(input) == 2:  # input[0] should be key and input[1] set of values
        if len(input[1]) > 0:
            # iterate over set of values 
            # input[1][i][0] is arrival delay
            # input[1][i][1] is departure delay
            m1 = np.median([val[0] for val in input[1] if val[2] == 1])
            m2 = np.median([val[1] for val in input[1] if val[3] == 1])
            return((input[0], m1, m2)) # m1, m2))
        else:
            return((input[0], -999, -999))
    else:
        return((input[0], -9999, -9999))


output = lines.map(computeKeyValue).groupByKey().cache()
medianResults = output.map(medianFun).collect()
medianResults[0:5]
# [(u'DL-8-PHL-LAX', 85.0, 108.0), (u'OO-12-IAH-CLL', -6.0, 0.0), (u'AA-4-LAS-JFK', 2.0, 0.0), (u'WN-8-SEA-GEG', 0.0, 0.0), (u'MQ-1-ORD-MDT', 3.0, 1.0)]

## @knitr spark-fit1

lines = sc.textFile('/data/airline')

def screen(vals):
    vals = vals.split(',')
    return(vals[0] != 'Year' and vals[14] != 'NA' and 
           vals[18] != 'NA' and vals[3] != 'NA' and
           float(vals[14]) < 720 and float(vals[14]) > (-30) )
# 0 field is Year
# 14 field is ArrDelay
# 18 field is Distance
# 3 field is DayOfWeek

lines = lines.filter(screen).repartition(192).cache()
# 192 is a multiple of the total number of cores: 24 (12 nodes * 2 cores/node)

n = lines.count()

import numpy as np
from operator import add

P = 8
bc = sc.broadcast(P)

#######################
# calc xtx and xty
#######################
def crossprod(line):
    vals = line.split(',')
    y = float(vals[14])
    dist = float(vals[18])
    dayOfWeek = int(vals[3])
    xVec = np.array([0.0] * P)
    xVec[0] = 1.0
    xVec[1] = float(dist)/1000
    if dayOfWeek > 1:
        xVec[dayOfWeek] = 1.0
    xtx = np.outer(xVec, xVec)
    xty = xVec * y
    return(np.c_[xtx, xty])

xtxy = lines.map(crossprod).reduce(add)
# 11 minutes

# now just solve system of linear equations!!

#######################
# calc xtx and xty w/ mapPartitions
#######################

# dealing with x matrix via mapPartitions

def readPointBatch(iterator):
    strs = list(iterator)
    matrix = np.zeros((len(strs), P+1))
    for i in xrange(len(strs)):
        vals = strs[i].split(',')
        dist = float(vals[18])
        dayOfWeek = int(vals[3])
        xVec = np.array([0.0] * (P+1))
        xVec[8] = float(vals[14]) # y
        xVec[0] = 1.0  # int
        xVec[1] = float(dist) / 1000
        if(dayOfWeek > 1):
            xVec[dayOfWeek] = 1.0
        matrix[i] = xVec
    return([matrix.T.dot(matrix)])

xtxyBatched = lines.mapPartitions(readPointBatch).reduce(add)
# 160 seconds

mle = np.linalg.solve(xtxy[0:P,0:P], xtxy[0:P,P])


## @knitr spark-fit2

def readPointPartition(iterator):
    strs = list(iterator)
    matrix = np.zeros((len(strs), P+1))
    print(len(strs))
    for i in xrange(len(strs)):
        vals = strs[i].split(',')
        dist = float(vals[18])
        dayOfWeek = int(vals[3])
        xVec = np.array([0.0] * (P+1))
        xVec[8] = float(vals[14]) # y
        xVec[0] = 1.0  # int
        xVec[1] = float(dist) / 1000
        if(dayOfWeek > 1):
            xVec[dayOfWeek] = 1.0
        matrix[i] = xVec
    return([matrix])

batches = lines.mapPartitions(readPointPartition).cache()
# 3 min

def denomSumSqPartition(mat):
    return((mat*mat).sum(axis=0))

def getNumPartition(mat):
    beta[p] = 0
    sumXb = mat[:, 0:P].dot(beta)
    return(sum((mat[:,P] - sumXb)*mat[:,p]))

sumx2 = batches.map(denomSumSqPartition).reduce(add)

beta = np.array([0.0] * P)
p = 0

oldBeta = beta.copy() # otherwise a shallow (i.e., pointer) copy!

it = 0

tol = .001
maxIts = 10
crit = 1e16

while crit > tol and it <= maxIts:
#for it in range(1,6):
    for p in xrange(P):
        # distribute current beta and current coordinate
        bc = sc.broadcast(beta)
        bc = sc.broadcast(p)
        # get numerator as product of residual and X for coordinate
        sumNum = batches.map(getNumPartition).reduce(add)
        beta[p] = sumNum / sumx2[p]   
        print("Updated var " + str(p) + " in iteration ", str(it), ".")
    crit = sum(abs(beta - oldBeta))
    oldBeta = beta.copy()  
    print("-"*100)
    print(beta)
    print(crit)
    print("-"*100)
    it = it+1

# 7 s per iteration;  ~9 minutes for 10 iterations
beta
#array([ 6.59246803,  0.76054724, -0.92357814,  0.16881708,  2.00073749,
#        2.66270618, -2.65116571, -0.36017589])



## @knitr spark-fit3

alpha = .4

def sumVals(mat):
    return(sum(mat[:,P]))

beta = np.array([0.0] * P)

beta[0] = batches.map(sumVals).reduce(add) / n
oldBeta = beta.copy()

bc = sc.broadcast(P)
bc = sc.broadcast(beta)

def getGradBatch(mat):
    sumXb = mat[:, 0:P].dot(beta)
    return( ((sumXb - mat[:,P])*((mat[:, 0:P]).T)).sum(1) )

def ssqObj(mat):
    return ( (pow(mat[:,P] - mat[:, 0:P].dot(beta), 2)).sum() ) 

objValue = batches.map(ssqObj).reduce(add)

nIts = 100

storeVals = np.zeros((nIts, P+2))
tol = .001
maxIts = 100
crit = 1e16

while crit > tol and it < maxIts:
    gradVec = batches.map(getGradBatch).reduce(add)
    beta = beta - alpha*gradVec / n
    crit = sum(abs(beta - oldBeta))
    bc = sc.broadcast(beta)
    objValue = batches.map(ssqObj).reduce(add)
    oldBeta = beta.copy()
    storeVals[it, 0] = pow(objValue/n,0.5)
    storeVals[it, 1] = crit
    storeVals[it, 2:(P+2)] = beta
    print("-"*100)
    print(it)
    print(beta)
    print(crit)
    print(pow(objValue/n,0.5))
    print("-"*100)
    it = it + 1

# 15 min
#[ 6.57348292  0.75335604 -0.9251238   0.16222806  1.98565752  2.64468325
# -2.63650861 -0.36507276]

## @knitr pyspark-script

import sys
from pyspark import SparkContext
from numpy import random as rand
if __name__ == "__main__":
    sc = SparkContext()
    # use sys.argv to get arguments
    # for example:
    total_samples = int(sys.argv[1]) if len(sys.argv) > 1 else 1000000
    num_slices = int(sys.argv[2]) if len(sys.argv) > 2 else 2
    samples_per_slice = round(total_samples / num_slices)
    def sample(p):
        rand.seed(p)
        x, y = rand.random(samples_per_slice), rand.random(samples_per_slice)
        # x, y = rand.random(samples_per_slice), 
        #   rand.random(samples_per_slice)
        return sum(x*x + y*y < 1)

    count = sc.parallelize(xrange(0, num_slices), num_slices).map(sample).reduce(lambda a, b: a + b)
    #count = sc.parallelize(xrange(0, num_slices), num_slices).
    # map(sample).reduce(lambda a, b: a + b)
    print "Pi is roughly %f" % (4.0 * count / (num_slices*samples_per_slice))

