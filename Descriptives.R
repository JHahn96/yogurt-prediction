head(df)
Namen<-names(table(df$article_name))
#split dataframe based on variabe
a<-df$article_name==Namen[1]
b<-df$article_name==Namen[2]
c<-df$article_name==Namen[3]
d<-df$article_name==Namen[4]
e<-df$article_name==Namen[5]
f<-df$article_name==Namen[6]
g<-df$article_name==Namen[7]
h<-df$article_name==Namen[8]
i<-df$article_name==Namen[9]
j<-df$article_name==Namen[10]
k<-df$article_name==Namen[11]
l<-df$article_name==Namen[12]
m<-df$article_name==Namen[13]
n<-df$article_name==Namen[14]
o<-df$article_name==Namen[15]
p<-df$article_name==Namen[16]
q<-df$article_name==Namen[17]
r<-df$article_name==Namen[18]
s<-df$article_name==Namen[19]

A<-df[a,]
B<-df[b,]
C<-df[c,]
D<-df[d,]
E<-df[e,]
FF<-df[f,]
G<-df[g,]
H<-df[h,]
I<-df[i,]
J<-df[j,]
K<-df[k,]
L<-df[l,]
M<-df[m,]
N<-df[n,]
O<-df[o,]
P<-df[p,]
Q<-df[q,]
R<-df[r,]
S<-df[s,]

Tot<-cbind(A$sales,B$sales,C$sales,D$sales,E$sales,FF$sales,G$sales,H$sales,I$sales,J$sales,K$sales,L$sales,M$sales,N$sales,O$sales,P$sales,Q$sales,R$sales,S$sales)
colnames(Tot)<-c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S")

library(psych)
fa(Tot)

Tot<-Tot/1000

fa.parallel(Tot)
Faktoren<-fa(Tot,nfactors=2)
Werte<-Faktoren$scores
MClassi<-Werte[,1]
Rest<-Werte[,2]



#plot(MClassi)
#plot(Rest)
#plot(A$sales)
#plot(B$sales)
#plot(C$sales)
#plot(D$sales)
#plot(E$sales)
#plot(FF$sales)
#plot(G$sales)
#plot(H$sales)
#plot(I$sales)
#plot(J$sales)

