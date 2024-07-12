import sqlite3
from datetime import date

con = sqlite3.connect('work.sqlite')
cur = con.cursor()

permtransf = cur.execute('select * from aih order by AIHREF,DT_INTER;')

dtint = open('consolida_episodios.csv', 'w')
dtint.writelines('AIHREF,NRECS,DT_INI,DT_FIM,DIAS,COBRANCA,PDIAG,PPROC,SOMA_UTI,SOMA_US,SOMA_CRIT,IDADE\n')

def todate(datestr: str) -> date:
    thedate = date(int(datestr[:4]),int(datestr[4:6]),int(datestr[6:]))
    return thedate

def datediff(mindate: str, maxdate: str) -> str: 
    delta = todate(maxdate) - todate(mindate)
    diffstr = str(delta).split(' ')[0]
    if diffstr == '0:00:00':
        diffstr = '0'
    return str(int(diffstr))

aihref: str = ''
nrecs: int = 0

nordem: int = 0
mindate: str  = ''
maxdate: str = ''
cobranca: str = ''
soma_uti: int = 0
soma_us: float = 0.0
soma_crit: int = 0
pdiag: str = ''
pproc: str = ''
idade: str = ''

for row in permtransf:
    nrecs += 1
    print(nrecs, end = '\r')
    if not aihref: # first
        aihref = row[8]
        mindate = row[4]
        maxdate = row[5]
        cobranca = row[6]
        soma_uti = int(row[13])
        soma_us = float(row[12].replace(',','.'))
        soma_crit = int(row[15])
        pdiag = row[10]
        pproc = row[11]
        idade = str(int(int(datediff(row[3],row[4]))/365.25))
        nordem = 1
    else:
        if aihref != row[8]: # new item
            dtint.writelines(aihref+','+str(nordem)+
                ','+mindate+','+maxdate+
                ','+datediff(mindate,maxdate)+
                ','+cobranca+','+pdiag+
                ','+pproc+','+str(soma_uti)+
                ','+str(soma_us)+','+str(soma_crit)+
                ','+idade+'\n')
            aihref = row[8]
            mindate = row[4]
            maxdate = row[5]
            cobranca = row[6]
            soma_uti = int(row[13])
            soma_us = float(row[12].replace(',','.'))
            soma_crit = int(row[15])
            pdiag = row[10]
            pproc = row[11]
            idade = str(int(int(datediff(row[3],row[4]))/365.25))
            nordem = 1
        else:
            if row[4] < mindate:
                mindate = row[4]
            if row[5] > maxdate:
                maxdate = row[5]
            soma_uti += int(row[13])
            soma_us += float(row[12].replace(',','.'))
            soma_crit += int(row[15])
            nordem += 1
            aihref = row[8]
            cobranca = row[6]
else: # for the last sequence
    dtint.writelines(aihref+','+str(nordem)+
        ','+mindate+','+maxdate+
        ','+datediff(mindate,maxdate)+
        ','+cobranca+','+pdiag+
        ','+pproc+','+str(soma_uti)+
        ','+str(soma_us)+','+str(soma_crit)+
        ','+idade+'\n')

dtint.close()
con.close()

print('\nFim.')
