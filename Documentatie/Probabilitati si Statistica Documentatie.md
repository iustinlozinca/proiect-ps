## Membri echipe:
- Irimia David
- Lungu Vlad
- Lozinca Iustin
- Rares
## Pachete suplimentare folosit:
- ggplot2 a fost folosit pentru a ajuta la afisarea graficelor.

# Enunt

## Analiza probabilistica a performantei unui serviciu online cu trafic aleator si impact economic
Se consideră o platformă online (aplicație web / API / microserviciu) care deservește utilizatori finali. Platforma procesează cereri (_requests_) venite de la un număr variabil de clienți, fiecare cerere fiind supusă incertitudinii: timpi de răspuns variabili, eșecuri temporare, reîncercări (_retry_), timeout-uri și _politici de backoff_.

#### Trafic zilnic
Numărul de clienți care accesează platforma într-o zi este aleator, notat $K_d = \text{numarul de clienti activi in ziua d}$, și este influențat de factori externi (sezonalitate, campanii, popularitate). Acest trafic determină încărcarea sistemului și, implicit, performanța
#### Procesarea cererilor

Fiecare client generează cereri. O cerere poate: reuși sau eșua, fi reluată de cel mult $N_{max}$ ori, avea timeout dacă depășește un prag de timp, aplica _backoff_ între _retry_-uri.

Pentru o cerere, definim:
- $S_i$ - timpul de raspuns la incercare $i$;
- $U_i \in {\{0,1\}}$ - succes/esec la incercarea $i$;
- $B_i$ -backoff intre incercari;
- $N$ - numarul total de incercari;
- $T$ - timpul total pana la succes sau abandon;
-  $I$ - indicator de succes final.
#### #### Experiența utilizatorului și churn
Un utilizator poate părăsi aplicația(churn): aleator, fără o cauză direct observabilă sau condiționat de performanță, de exemplu dacă, într-o fereastră de timp sau într-un număr de cereri consecutive, prea multe cereri nu sunt rezolvate.

#### Impact economic
Fiecare cerere reușită produce un câstig, fiecare utilizator pierdut produce o pierdere (cost de achiziție + venituri viitoare ratate), iar nerespectarea SLA poate produce penalități.

**Scopul proiectului** este de a înțelege, prin modelare probabilistică și prin simulare în `R`, relația dintre trafic, performanță tehnică și impact economic.

# Exercitii si rezolvari
### 1. Modelarea traficului zilnic (variabile aleatoare discrete)

##### Cerinta
a. Modelați  $K_d$ folosind, pe rând, cel puțin două distribuții discrete (ex.: Poisson, Binomială).

b. Generați prin simulare eșantioane mari care să reprezinte traficul zilnic pentru o perioadă de câțiva ani și reprezentați histogramele asociate acestora. Interpretați comparativ histogramele obținute pe luni și pe ani.

c. Estimați empiric media și varianța traficului pentru fiecare an și comparați cu valorile teoretice.

d. Interpretați diferențele între modele (trafic redus vs plafonat).
##### Rezolvare:

Pentru a modela numarul de clienti activi intr-o zi ($K_d$), am utilizat distributia Poisson si cea Binomiala, fiecare reprezentand un scenariu diferit de trafic asupra platformei.

**a) Modelele utilizate:**
**Modelul Poisson (Trafic Nelimitat):**
    - Am presupus ca numarul de cereri vine dintr-o populatie teoretic infinita, unde evenimentele (sosirea clientilor) sunt independente.
    - Parametrul $\lambda$ reprezinta media numarului de clienti pe zi.
    - functia: `rpois(n_zile, lambda = input$lambda)`
    
**Modelul Binomial (Trafic Plafonat):**
    - Am presupus un numar fix de utilizatori totali inregistrati ($n_{max})$), fiecare avand o probabilitate $p$ de a fi activ intr-o zi data.
    - functia: `rbinom(n, size, prob)`.
#notita aici in enunt ne cere sa interpretam diferenta intre trafic redus si trafic plafonat dar noi avem trafic nelimitat si trafic plafonat?

**b/c) Simulare si Histograme:**

![[ex_1_b.png]](imagini/ex_1_b.png)
- Observatii grafice:        
    - Pentru perioade lungi (cativa ani), conform Legii Numerelor Mari, histograma converge catre forma teoretica a distributiei de probabilitate.

![[ex_1_b_histograma_1.png]](imagini/ex_1_b_histograma_1.png)
![[ex_1_b_histograma_2.png]](imagini/ex_1_b_histograma_2.png)
**c) Estimarea mediei si variantei:**

Aplicatia calculeaza automat parametrii empirici si ii compara cu cei teoretici in tab-ul "Statistici Comparative".

Formulele utilizate pentru validare sunt:

- **Poisson:**
    - Teoretic: $E[X] = \lambda$, $Var(X) = \lambda$.
    - Empiric: `mean(esantion)` si `var(esantion)`.
- **Binomial:**
    - Teoretic: $E[X] = n \cdot p$, $Var(X) = n \cdot p \cdot (1-p)$.
    - Empiric: Media si varianta de selectie calculate pe vectorul generat.
        

S-a observat ca pentru $N$, diferentele dintre valorile empirice si cele teoretice scad semnificativ.

**d) Interpretarea diferentelor intre modele:**

Diferenta fundamentala intre cele doua abordari consta in **variabilitate si limite**:

1. **Dispersia (Varianta):** In modelul Poisson, varianta este egala cu media. In modelul Binomial, varianta este intotdeauna mai mica decat media (deoarece $1-p < 1$). Asta inseamna ca traficul "plafonat" este mai predictibil si fluctueaza mai putin in jurul mediei decat traficul "nelimitat".
    
2. **Valorile Extreme:** Modelul Poisson permite, teoretic, valori oricat de mari (coada distributiei este infinita), ceea ce modeleaza riscul unor varfuri neasteptate de trafic (viralitate). Modelul Binomial este taiat la dreapta la valoarea $n$, deci sistemul nu poate fi supraincarcat peste capacitatea bazei de clienti.

### 2. Modelarea timpilor de răspuns (variabile aleatoare continue)
##### Cerinta
 
a. Modelați $S$, pe rând, cu o distribuție asimetrică (Exponențială/Gamma) și respectiv cu o distribuție Normală (eventual trunchiată la valori pozitive).

b. Construiți histogramele pentru $S$ și suprapuneți peste acestea densitățile teoretice.

c. Calculați media, varianța, mediana, valoarea modală și interpretați rezultatele obținute.

d. Discutați diferența dintre medie și mediană în contextul latențelor.

#### Rezolvare:
**a) Modelele utilizate:**

**Distributia Exponentiala (Asimetrica):**
    - Aceasta este distributia clasica pentru modelarea timpilor de asteptare sau de servire intr-un sistem de cozi (M/M/1).
    - Este definita de un singur parametru $\lambda$ (rata), unde $E[S] = 1/\lambda$.
    - In cod, utilizatorul introduce media dorita (in milisecunde), iar aplicatia calculeaza $\lambda = 1 / \text{media}$.
 **Distributia Normala (Simetrica):**
    - Caracterizata de medie ($\mu$) si deviatie standard ($\sigma$).
    - Deoarece distributia normala teoretica este definita pe $(-\infty, +\infty)$, iar timpul nu poate fi negativ, in simulare am aplicat o **trunchiere**: am eliminat valorile generate care erau mai mici sau egale cu 0 (`s_norm[s_norm > 0]`).

**b) Histograme si Densitati:**

In tab-ul "Distributii (Histograme)", am generat esantioane de dimensiune $N$ si am reprezentat datele grafic:
	 -**Histograma:** Arata frecventa empirica a timpilor de raspuns simulati.
	- **Densitatea Teoretica:** Am suprapus curba functiei de densitate de probabilitate (PDF) - linia rosie pentru Exponential si linia albastra pentru Normal.

Suprapunerea vizuala valideaza simularea: barele histogramei urmaresc fidel linia curbei teoretice, confirmand corectitudinea generarii datelor.

![[ex_2_histograma_1.png]](imagini/ex_1_b_histograma_1.png)
![[ex_2_histograma_2.png]](imagini/ex_1_b_histograma_2.png)
**c) Statistica descriptiva (Medie, Varianta, Mediana, Mod):**

In tab-ul "Statistici Comparative", aplicatia calculeaza indicatorii cheie.

Un aspect tehnic de mentionat este calculul **modului** (valoarea cea mai frecventa) pentru date continue empirice. Deoarece probabilitatea ca o variabila continua sa ia exact o valoare specifica este 0, am estimat modul prin calcularea maximului functiei de densitate empirica (`density(v)` in R).

Comparatia evidentiaza comportamentul diferit al celor doua modele:

- La **Normala**, Media $\approx$ Mediana $\approx$ Modul (distributie simetrica).
    
- La **Exponentiala**, Modul este aproape de 0 (cele mai multe cereri sunt foarte rapide), Mediana este mai mica decat Media.
    
![[ex_2_c.png]](imagini/ex_2_c.png)
**d) Interpretarea diferentei Medie vs. Mediana in contextul latentei:**

1. Asimetria la dreapta: Distributia timpilor de raspuns reali (si cea Exponentiala) este asimetrica pozitiv (skewed right).
    
2. Impactul outlier-ilor: Cateva cereri foarte lente trag Media in sus semnificativ.
    
3. Experienta utilizatorului tipic: Mediana este un indicator mult mai robust pentru experienta "utilizatorului obisnuit". Faptul ca media este mult mai mare decat mediana (in exemplul nostru, $Mean > Median$) ne spune ca desi majoritatea clientilor au o experienta buna, sistemul sufera de instabilitate ocazionala care afecteaza media globala.

### 3. Cereri, retry-uri si evenimente

##### Cerinta:
Definiți evenimentele:
$$
\begin{array}
\\ A& =& \{ I = 1 \} \text{(succes)}
\\ B& =& \{ T \le t_0 \} \text{(SLA)}
\\ C& =& \{ N \le n_0 \}
\\ D& =& \text{\{cel putin un esec\}}
\end{array}
$$

a. Estimați empiric $\mathbb{P}(A), \mathbb{P}(B), \mathbb{P}(C), \mathbb{P}(A \cap B), \mathbb{P}(A \cup D)$

b. Verificați numeric formulele pentru reuniune/intersecție

c. Explicați de ce probabilitatea empirică aproximează bine probabilitatea teoretică.

##### Rezolvare:

Logica simularii pentru fiecare client (din cei $N_{sim}$ simulati) este urmatoarea:

1. Se incearca efectuarea cererii.
    
2. Timpul de raspuns pentru o incercare este generat exponential (`rexp` cu rata 2, adica medie 0.5 secunde).
    
3. Daca cererea reuseste (cu probabilitatea $p$), procesul se opreste ($I=1$).
    
4. Daca cererea esueaza, se adauga un timp de penalizare (_backoff_ constant de 0.2 secunde) si se incrementeaza contorul de incercari.
    
5. Procesul se repeta pana la succes sau pana la atingerea numarului maxim de retry-uri ($N_{max}$).
    

**a) Estimarea empirica a probabilitatilor:**

Definirea evenimentelor in cod s-a facut astfel:

- **A (Succes):** Variabila indicator $I = 1$.
    
- **B (SLA - Service Level Agreement):** Timpul total $T \le t_0$ (unde $t_0$ este selectat din interfata).
    
- **C (Eficienta):** Numarul de incercari $N \le n_0$.
    
- **D (Cel putin un esec):** Acest eveniment are loc daca nu am avut succes din prima incercare. In cod: `!(N == 1 & I == 1)`. Aceasta conditie acopera atat situatia in care am avut succes dupa mai multe incercari, cat si situatia in care am esuat total.
![[ex_3_1.png]](imagini/ex_3_1.png)
Probabilitatile au fost estimate folosind **frecventa relativa**.

Exemplu: `mean(df$T <= input$t0_SLA)` calculeaza $P(B)$.

**b) Verificarea numerica a formulelor (Reuniune/Intersectie):**

In tab-ul "Verificare Formule", am validat **Principiul Incluziunii si Excluderii** pentru doua evenimente:

$$P(A \cup D) = P(A) + P(D) - P(A \cap D)$$

Calculand separat fiecare termen pe baza datelor simulate, am aratat ca egalitatea se pastreaza (cu o marja de eroare infima data de precizia reprezentarii in virgula mobila).

De exemplu, $P(A \cap D)$ reprezinta probabilitatea ca cererea sa reuseasca in final, dar sa fi avut cel putin un esec pe parcurs (succes cu retry).

**c) De ce probabilitatea empirica aproximeaza bine probabilitatea teoretica**
Aceasta este o aplicatie directa a **Legii Numerelor Mari (Law of Large Numbers)** .

Legea spune ca media esantionului (frecventa relativa a evenimentului) converge probabilistic catre media populatiei (probabilitate teoretica) atunci cand dimensiunea esantionului (n) creste.

**Grafice si Vizualizare:**


- **Histograma Timpului Total (T):** Arata distributia timpilor de asteptare. Se poate observa adesea o distributie multimodala (mai multe "cocoase"), fiecare mod corespunzand numarului de incercari (timpul pentru 1 incercare, timpul pentru 2 incercari + backoff, etc.).
    
- **Graficul de bare pentru N:** Arata cati utilizatori au reusit din prima, cati din a doua, etc.
![[ex_3_histograme.png]](imagini/ex_3_histograme.png)
### 4. Variabile aleatoare bidimensionale discrete
##### Cerinta
Considerați variabila bidimensională $(N,F)$, unde $F$  este numărul de eșecuri. Determinați:

a. Distribuția comună empirică;

b. Distribuțiile marginale;

c. Un test empiric de independență;

d. O modalitate de vizualizare (tabel/heatmap) și interpretare.
##### Rezolvare:

Deoarece $F$ este o componenta a lui $N$ (relatia fiind $N = F + I_{succes}$), ne asteptam la o dependenta puternica intre cele doua. In plus, am introdus in simulare conceptul de **Churn (Abandon)**, care poate fi:

1. **Aleator:** Utilizatorul renunta cu o probabilitate $q$ dupa orice esec, indiferent de istoric.
    
2. **Conditionat:** Utilizatorul renunta daca intampina un numar de esecuri _consecutive_ (frustrare).

Am utilizat o simulare Monte Carlo pentru a genera perechi $(n_i, f_i)$.

**a) si d) Distributia comuna empirica si Vizualizare (Heatmap):**

Distributia comuna este definita ca probabilitatea $P(N=n, F=f)$.

Am construit tabelul folosind functia `table(df$Trials, df$Failures)` si l-am normalizat pentru a obtine probabilitati.

Am ales sa folosim un heatmap pentru ca acesta:
- Evidentiaza zonele cu probabilitate maxima (culori mai intense).
    
- Arata structura suportului distributiei (de exemplu, observam ca probabilitatea este 0 pentru $F \ge N$, deoarece nu putem avea mai multe esecuri decat incercari, ceea ce se vede prin zona alba de sub diagonala).
    

![[ex_4_heatmap.png]](imagini/ex_4_heatmap.png)
**b) Distributiile marginale:**

Distributiile marginale se obtin prin insumarea probabilitatilor pe linii (pentru $N$) sau pe coloane (pentru $F$).

- **Marginala lui N:** Ne arata cat de persistenti sunt utilizatorii in general.
    
- **Marginala lui F:** Ne arata rata de eroare perceputa de utilizatori.
    

In cod, acestea sunt reprezentate prin grafice de bare (Bar Plots) separate. Prezenta ratei de abandon modifica forma acestor distributii, deplasand masa de probabilitate catre valori mai mici (utilizatorii renunta mai repede).

![[ex_4_Distributii_Marginale.png]](imagini/ex_4_Distributii_Marginale.png)
**c) Test empiric de independenta:**

Pentru a verifica statistic daca numarul de incercari ($N$) este independent de numarul de esecuri ($F$), am aplicat testul Chi-patrat de independenta.

Ipoteza nula ($H_0$) este ca variabilele sunt independente.

Rezultatul testului (afisat in consola aplicatiei) returneaza un **p-value** extrem de mic (aproape de 0).

![[ex_4_independenta.png]](imagini/ex_4_independenta.png)
**Interpretare:**
Valoarea $p-value < 0.05$ ne obliga sa respingem ipoteza nula. Concluzia este ca **N si F sunt variabile dependente**.

 Daca un utilizator are multe esecuri ($F$ mare), automat numarul de incercari ($N$) trebuie sa fie mare.

 Daca sistemul impune o limita $N_{max}$, $F$ este limitat superior de $N$.
### 5. Variabile aleatoare bidimensionale (discrete si continue)
##### Cerinta
Considerați variabila bidimensională $(N,T)$.

a. Reprezentați grafic variabila bidimensională $(N,T)$ .

b. Calculați mediile, varianțele, covarianța și coeficientul de corelație

c. Interpretați corelația (retry-uri vs latență totală).

##### Rezolvare:

In acest exercitiu, analizam cuplul $(N, T)$, unde:
- $N$ este o variabila **discreta** (numarul de incercari: 1, 2, ... $N_{max}$).
- $T$ este o variabila **continua** (timpul total scurs pana la finalizarea procesului sau abandon).

**a) Reprezentarea grafica a variabilei bidimensionale:**

Vizualizarea relatiei dintre o variabila discreta si una continua ridica probleme specifice. Daca am folosi un _Scatterplot_ simplu, toate punctele s-ar suprapune pe linii verticale (in dreptul lui $N=1, N=2$, etc.).

Pentru a rezolva acest lucru, am implementat doua tipuri de grafice:

**Scatterplot cu Jitter:** Am adaugat un mic "zgomot" aleator pe axa X pentru a dispersa punctele si a vizualiza densitatea observatiilor. Altfel punctele ar fii fost suprapuse.

**Boxplot:** Acesta este cel mai potrivit pentru a vizualiza distributia unei variabile continue ($T$) grupata dupa o variabila discreta ($N$).

![[ex_5_1.png]](imagini/ex_5_1.png)
![[ex_5_2.png]](imagini/ex_5_2.png)
**b) Indicatori statistici (Medii, Variante, Covarianta, Corelatie):**

Aplicatia calculeaza parametrii descriptivi pentru $N$ si $T$ separat, dar si indicatorii care descriu legatura dintre ele:

- **Covarianta ($Cov(N, T)$):** Masoara directia relatiei liniare. O valoare pozitiva indica faptul ca variabilele tind sa creasca impreuna.
    
- **Coeficientul de corelatie Pearson ($\rho$):** Este versiunea normalizata a covariantei, cu valori in intervalul $[-1, 1]$. Acesta ne arata cat de puternica este legatura liniara.

![[ex_5_3.png]](imagini/ex_5_3.png)

**c) Interpretarea corelatiei (Retry-uri vs. Latenta):**

Analizand rezultatele simulate, observam o **corelatie pozitiva puternica** (de obicei $\rho > 0.5$).

Interpretarea fizica este urmatoarea:

1. **Cauzalitate:** Fiecare incercare suplimentara ($N$ creste) adauga in mod obligatoriu timp la total ($T$ creste), atat prin durata procesarii cererii, cat si prin timpul de _backoff_.
    
2. **De ce nu este 1?** Corelatia nu este perfecta ($\rho \neq 1$) din cauza aleatoriului intrinsec al distributiei exponentiale. Este posibil ca un client cu o singura incercare ($N=1$) sa astepte mai mult (daca nimereste o valoare extrema din coada distributiei exponentiale) decat un client cu 2 incercari rapide.
### 6. Probabilitati conditionate si conditionari
##### Cerinta:
a. Estimati $\mathbb{P}(A|N \le n_0), \mathbb{P}(B|A)$.
b. Calculat: $\mathbb{E}(T|I = 1),\mathbb{E}(T|I = 0)$
c. Interpretați rezultatele din perspectiva experienței utilizatorului.
##### Rezolvare:
In simularea Monte Carlo, estimarea probabilitatilor conditionate se realizeaza prin filtrarea (subsetarea) datelor: calculam frecventa evenimentului A doar in cadrul subsetului de date unde conditia B este adevarata.

**a) Estimarea probabilitatilor conditionate:**

Am estimat doua marimi esentiale pentru analiza performantei:

1. $P(A \mid N \le n_0)$: **Probabilitatea de succes conditionata de "eficienta"**.
    
    - Aceasta masoara daca cererile care se rezolva din putine incercari (sub pragul $n_0$) au o sansa mai mare de succes final.
        
    - In R: `mean(df$I[df$N <= n0] == 1)`.
        
2. $P(B \mid A)$: **Probabilitatea de a respecta SLA, dat fiind ca cererea a reusit**.
    
    - Aceasta este o metrica de calitate a serviciului (QoS). Nu ne intereseaza daca cererile esuate au depasit timpul (oricum au esuat), ci ne intereseaza "Dintre cei care au primit un raspuns, cati l-au primit rapid?".
        
    - In R: `mean(df$Timp[df$I == 1] <= t0)`.
        

![[ex_6_1.png]](imagini/ex_6_1.png)

**b) Sperante conditionate (Media timpului in functie de rezultat):**

Speranta conditionata $E[T | I=i]$ reprezinta media timpului $T$, calculata separat pentru grupul succeselor ($I=1$) si grupul esecurilor ($I=0$).

- $E[T \mid I=1]$: Timpul mediu asteptat de un utilizator multumit.
    
- $E[T \mid I=0]$: Timpul mediu pierdut de un utilizator care, in final, nu primeste serviciul.
    

Vizualizarea s-a facut prin histograme suprapuse si boxplot-uri, care arata clar separarea distributiilor. Esecurile sunt, de obicei, concentrate in partea dreapta a graficului (timpi mari).

![[ex_6_2.png]](imagini/ex_6_2.png)

**c) Interpretarea rezultatelor din perspectiva experientei utilizatorului (UX):**
![[ex_6_3.png]](imagini/ex_6_3.png)
### 7. Independenta vs dependenta
##### Cerinta:
a. Simulați două scenarii: timpi $S_i$  independenți vs dependenți (latența crește după eșecuri).
b. Comparați distribuția și varianța lui $T$  în cele două scenarii.
c. Formulați concluzii privind riscul și stabilitatea sistemului.
##### Rezolvare:

In acest exercitiu, analizam impactul dependentei dintre esecuri si timpul de raspuns asupra stabilitatii sistemului. Am comparat doua scenarii distincte prin simulare:

1. **Scenariul Independent (Stabil):** Timpul de raspuns pentru fiecare incercare este generat dintr-o distributie Exponentiala cu parametru constant $\lambda$. Faptul ca o cerere anterioara a esuat nu afecteaza performanta serverului pentru urmatoarea incercare.
    
2. **Scenariul Dependent (Instabil / Congestie):** Aici modelam un sistem care se degradeaza sub sarcina. Daca o cerere esueaza, presupunem ca sistemul este incarcat, deci urmatoarea incercare va avea o rata de servire mai mica ($\lambda_{nou} = \lambda_{vechi} \cdot k$, unde $k < 1$ este factorul de degradare). Aceasta duce la cresterea timpului mediu de asteptare pentru incercarile ulterioare.

Utilizeam doua bucle de simulare paralele pentru a genera vectorii de timpi totali $T$ pentru ambele scenarii.

**a) Simulare si Comparatie Grafica:**

In tab-ul "Comparatie Distributii", am suprapus densitatile empirice ale celor doua scenarii.

- **Curba Verde (Independent):** Este mai ascutita si concentrata in jurul mediei.
    
- **Curba Rosie (Dependent):** Este mai plata ("turtita") si are o "coada lunga" spre dreapta (valori extreme mari).
    

Aceasta vizualizare demonstreaza ca, desi multi utilizatori pot avea timpi similari in ambele scenarii (cei care reusesc din prima), utilizatorii ghinionisti din scenariul dependent sufera intarzieri mult mai mari.

![[ex_7_1.png]](imagini/ex_7_1.png)

**b) Comparatia distributiei si a variantei lui T:**

Analiza cantitativa din tab-ul "Varianta si Statistici" confirma observatiile vizuale.

Elementul cheie este **Varianta ($Var(T)$)**. In scenariul dependent, varianta explodeaza.

Motivul este efectul de "bulgare de zapada": un esec atrage dupa sine un timp de raspuns mai mare, care creste probabilitatea de timeout, care duce la un nou retry si mai lent.

Acest fenomen creaza o distributie cu **Heavy Tails** (cozi grele), unde valorile extreme (outliers) sunt mult mai frecvente decat intr-o distributie exponentiala standard.

![[ex_7_2.png]](imagini/ex_7_2.png)

**c) Concluzii privind riscul si stabilitatea sistemului:**

Pe baza rezultatelor, putem formula urmatoarele concluzii de arhitectura software:

![[ex_7_3.png]](imagini/ex_7_3.png)
### 8. Inegalitati probabilistice (garantii worst-case)
##### Cerinta
Pentru $T \ge 0$:

a. Verificați numeric inegalitățile Markov și Cebîșev (empiric versus teoretic).

b. Pentru variabila număr de eșecuri/încercări verificați o inegalitate de tip Chernoff.

c. Interpretați utilitatea acestor limite când distribuțiile exacte sunt necunoscute.

d. Pentru o funcție convexă $\varphi$(ex.:$x^2,e^x$) verificați numeric $\varphi(\mathbb{E}(T)) \le \mathbb{E}(\varphi(T))$  (inegalitatea lui Jensen)

e. Interpretați rezultatul de la d) în contextul riscului (penalizarea valorilor extreme).


##### Rezolvare:

Verificam numeric patru inegalitati folosind simulari Monte Carlo:

1. Pentru $T$ (Timp) am folosit o distributie continua **Gamma(3, 2)** (pentru Markov, Cebisev, Jensen).
    
2. Pentru $X$ (Numar esecuri) am folosit o distributie discreta **Binomiala(n, p)** (pentru Chernoff).
    

**a) Inegalitatile Markov si Cebisev:**

- **Inegalitatea lui Markov:** Pentru o variabila pozitiva $T \ge 0$, $P(T \ge a) \le \frac{E[T]}{a}$.
    
    - Aceasta este cea mai "slaba" inegalitate, dar necesita cele mai putine informatii (doar media).
        
    - Comparam procentul de valori simulate care depasesc pragul $a$ cu valoarea teoretica $Media / a$.
        
- **Inegalitatea lui Cebisev:** $\mathbb{P}(|T - \mu| \ge k\sigma) \le \frac{1}{k^2}$.
    
    - Aceasta ne spune ca valorile aflate la $k$ deviatii standard distanta de medie sunt improbabile.
        
    - De exemplu, pentru $k=2$, cel mult $1/4$ (25%) din date pot fi in afara intervalului, indiferent de distributie.
        

![[ex_8_1.png]](imagini/ex_8_1.png)

**b) Inegalitatea lui Chernoff:**

Aceasta se aplica sumelor de variabile independente (cum este distributia Binomiala, suma de variabile Bernoulli). Ofera limite mult mai stranse (care scad exponential) decat Cebisev.

In simulare, verificam probabilitatea ca numarul de esecuri sa depaseasca media cu un procent $\delta$:

$$\mathbb{P}(X \ge (1+\delta)\mu)$$

Limita teoretica calculata in cod este mult mai mica, demonstrand puterea acestei inegalitati pentru analiza riscului de "avalanse" de erori.

![[ex_8_2.png]](imagini/ex_8_2.png)
**c) Utilitatea acestor limite:**

Atunci cand distributiile exacte sunt necunoscute:

1. **Worst-case analysis:** Aceste inegalitati ne permit sa dam garantii de tipul "Sistemul nu va depasi timpul critic in mai mult de X% din cazuri", chiar daca nu stim exact cum se comporta traficul, atat timp cat ii cunoastem media si varianta.
    
2. **Ierarhie:** Markov este utila pentru limite grosiere. Cebisev este utila pentru controlul variatiei. Chernoff este esentiala pentru dimensionarea serverelor (capacitate), deoarece erorile tind sa se medieze in timp, iar deviatiile extreme sunt exponential de rare.
    

**d) Inegalitatea lui Jensen:**

Pentru o functie convexa $\varphi$ (cum ar fi $x^2$ sau $e^x$), inegalitatea afirma:

$$\varphi(E[T]) \le E[\varphi(T)]$$

In aplicatie, am vizualizat acest lucru geometric. Punctul rosu (Media valorilor transformate) este intotdeauna mai sus decat punctul albastru (Valoarea transformata a mediei).

![[ex_8_3.png]](imagini/ex_8_3.png)

**e) Interpretarea in contextul riscului:**

![[ex_8_4.png]](imagini/ex_8_4.png)
### 9. Aproximare normala si agregare
##### Cerinte:

a. Pentru sume/agregări zilnice (ex.: total latență pe zi sau profit zilnic), studiați oportunitatea aproximării cu o distribuție normală prin simulare.
b. Comparați histograma agregatului cu o normală ajustată și precizați când aproximarea este adecvată.

##### Rezolvare:

Simulam procesul:

1. Generam $n$ cereri pe zi (unde $n$ este selectabil, ex: 100).
    
2. Fiecare cerere are o latenta generata dintr-o distributie asimetrica (Exponentiala sau Gamma) sau Uniforma.
    
3. Calculam latenta totala zilnica prin insumare.
    
4. Repetam procesul pentru un numar mare de zile pentru a obtine distributia agregatului.

**a) Oportunitatea aproximarii cu o distributie normala:**

In tab-ul "Agregare Zilnica", vizualizam histograma sumelor zilnice.

Aplicatia calculeaza statistici descriptive:

- **Asimetria (Skewness):** Pentru o distributie Normala perfecta, aceasta trebuie sa fie 0. Daca distributia latentei individuale este puternic asimetrica (ex: Exponentiala) si $n$ este mic (putine cereri pe zi), agregatul va pastra o usoara asimetrie.
    
- **Media si Varianta:** Conform teoriei, daca $X_i$ are media $\mu$ si varianta $\sigma^2$, suma $S_n = \sum X_i$ va avea media $n\mu$ si varianta $n\sigma^2$. Tabelul din aplicatie confirma ca valorile empirice sunt foarte apropiate de aceste valori teoretice.
    

![[ex_9_1.png]](imagini/ex_9_1.png)
**b) Comparatie histograma vs. Normala ajustata:**

In tab-ul "Comparatie cu Normala", verificam vizual si statistic validitatea aproximarii.

1. **Suprapunere Grafica:** Linia rosie (Normala teoretica) este suprapusa peste histograma verde. Cand numarul de cereri pe zi ($n$) este mare (ex: $>30-50$), suprapunerea este aproape perfecta.
    
2. **Q-Q Plot (Quantile-Quantile):** Acesta este un instrument vizual de diagnostic. Daca punctele albastre se aliniaza pe linia rosie diagonala, datele sunt distribuite normal. Deviatiile la capete indica "cozi" mai grele sau mai usoare decat ale normalei.
    
3. **Testul Shapiro-Wilk:** Un test statistic riguros. Un `p-value > 0.05` ne spune ca nu putem distinge statistic datele noastre de o distributie normala (deci aproximarea este excelenta).
    

![[ex_9_2.png]](imagini/ex_9_2.png)

**Interpretare si Concluzie:**

- **Cand este aproximarea adecvata?** Simularea arata ca pentru $n$ mic (ex: 10 cereri/zi) si distributie exponentiala, histograma este inca vizibil asimetrica ("skewed right"). In acest caz, aproximarea cu Normala este riscanta (subestimeaza cozile).
    
- **Impactul volumului:** Pe masura ce crestem $n$ (trafic intens), distributia devine simetrica (Clopotul lui Gauss). Aceasta justifica de ce in rapoartele lunare sau anuale folosim adesea presupunerea de normalitate pentru indicatori agregati (KPIs), simplificand enorm calculele de risc si intervalele de incredere.
### 10. Churn (pierderea utilizatorilor)
##### Cerinta:
Pierderea utilizatorilor se realizează prin două mecanisme: _aleator_ (cu o probabilitate constantă  ) și respectiv _condiționat_, dacă într-o fereastră de $m$ cereri, cel puțin $k$ eșuează.

a. Modelați probabilistic cele două scenarii.
b. Estimați probabilitatea de pierdere a utilizatorului.
c. Comparați scenariile și interpretați.

##### Rezolvare:

Analiza de _Churn_ (rata de abandon) este esentiala pentru sustenabilitatea economica a platformei. In acest model, am considerat ca un utilizator poate parasi sistemul din doua motive distincte, care actioneaza simultan (model de tip _competing risks_):

- **Churn Aleator (Mecanismul 1):** Utilizatorul pleaca din motive externe sistemului (ex: a gasit o oferta mai buna, nu mai are nevoie de serviciu). Acesta este modelat ca un proces Bernoulli cu probabilitatea $q$ la fiecare pas de timp.
- **Churn Conditionat de Performanta (Mecanismul 2):** Utilizatorul pleaca din cauza frustrarii tehnice. Regula implementata este: daca intr-o fereastra glisanta de $m$ cereri recente, utilizatorul intampina $k$ sau mai multe esecuri, acesta abandoneaza serviciul.

Simuleam traiectoria fiecarui utilizator pe un orizont de timp $H$. La fiecare pas, verificam daca a avut loc un eveniment aleator sau daca s-a activat conditia de erori multiple.

**a) Modelarea probabilistica a celor doua scenarii:**

In tab-ul "Vizualizare", graficul **Curba de Supravietuire** (Survival Curve) arata procentul de utilizatori care raman activi in timp.

- Porneste de la 100% ($t=0$).
    
- Scade monoton pe masura ce utilizatorii parasesc sistemul.
    
- Panta curbei indica rata de pierdere: o panta abrupta inseamna o problema grava de retentie.
    

![[ex_10_1.png]](imagini/ex_10_1.png)

**b) Estimarea probabilitatii de pierdere a utilizatorului:**

Probabilitatea totala de churn pe orizontul $H$ este estimata empiric prin raportul:

$$P(\text{Churn}) = \frac{\text{Numar utilizatori pierduti}}{\text{Numar total utilizatori}}$$

Aplicatia descompune aceasta pierdere in functie de cauza principala. Graficul de bare "Distributia Cauzelor de Churn" ne arata cati utilizatori au fost pierduti din cauza "ghinionului" (aleator) si cati din cauza erorilor tehnice (conditionat).

![[ex_10_2.png]](imagini/ex_10_2.png)

**c) Comparatie si Interpretare:**

In tab-ul "Interpretare & Comparatie", analizam care dintre cele doua mecanisme domina. Acest lucru dicteaza strategia de business:

1. **Daca domina Churn-ul Conditionat:** Inseamna ca stabilitatea tehnica a platformei este slaba. Probabilitatea de eroare $p$ este prea mare, sau pragul de toleranta al utilizatorilor ($k$) este atins prea des. Solutia este strict tehnica (repararea bug-urilor, scalarea serverelor).
    
2. **Daca domina Churn-ul Aleator:** Inseamna ca produsul functioneaza tehnic, dar utilizatorii nu sunt loiali..
    
3. **Efectul Ferestrei ($m$):** O fereastra de monitorizare mai mare ($m$) creste sansa de churn conditionat, deoarece "memoria" utilizatorului asupra erorilor este mai lunga.
    

![[ex_10_3.png]](imagini/ex_10_3.png)
### 11. Impact economic
##### Cerinta:
a. Definiți o v.a. pentru profitul zilnic(câștig per succes, pierdere per churn, penalități SLA).
b. Estimați media, varianța, și (opțional) intervale de încredere pentru profit.
c. Analizați compromisurile tehnico-economice.

##### Rezolvare:

Obiectivul final al oricarei platforme comerciale este profitabilitatea. In acest exercitiu, am transformat evenimentele tehnice (succese, erori, latente, churn) in valori monetare pentru a studia **Profitul Zilnic ($P$)** ca o variabila aleatoare.

Simuleaza activitatea pe o perioada de un an (sau numarul de zile ales), calculand profitul pentru fiecare zi in parte pe baza formulei:

$$P = (N_{succes} \cdot G) - (N_{churn} \cdot C_{churn}) - (N_{SLA} \cdot C_{SLA})$$

Unde:

- $G$ = Castigul pentru o cerere reusita.
- $C_{churn}$ = Costul pierderii unui client (de obicei foarte mare, include costul de achizitie + LTV).
- $C_{SLA}$ = Penalitatea pentru depasirea timpului de raspuns (SLA).

**a) Definitia variabilei pentru profitul zilnic:**

Profitul este o suma de variabile aleatoare, fiind influentat de patru factori de incertitudine simultani:

1. **Volumul de trafic:** Numarul de cereri variaza zilnic (modelat Poisson).
    
2. **Rata de succes:** Determina cate cereri aduc bani (modelat Binomial).
    
3. **Performanta (Latenta):** Determina penalitatile SLA (modelat Exponential).
    
4. **Retentia (Churn):** Determina pierderile majore ocazionale (modelat Bernoulli/Binomial).
    

In tab-ul "Profitul Zilnic", histograma arata distributia acestui profit. Linia rosie indica media, iar linia neagra indica pragul de rentabilitate ("breakeven", $P=0$).

![[ex_11_1.png]](imagini/ex_11_1.png)

**b) Estimari statistice (Medie, Varianta, Intervale de Incredere):**

Investitorii sunt interesati nu doar de profitul mediu, ci si de risc (variabilitate).

- **Media ($E[P]$):** Ne arata profitabilitatea pe termen lung.
    
- **Varianta ($Var[P]$):** Ne arata volatilitatea. O varianta mare inseamna ca zilele foarte profitabile alterneaza cu zile cu pierderi mari.
    
- **Intervalul de Incredere 95%:** Calculat in aplicatie, acesta ne ofera un interval in care ne asteptam sa se afle adevarata medie a profitului zilnic.
    
- **Profitul Cumulat:** Graficul liniei ascendente arata sanatatea financiara in timp. Daca linia coboara, firma pierde bani constant.
    

![[ex_11_2.png]](imagini/ex_11_2.png)
**c) Analiza compromisurilor tehnico-economice:**

Aceasta este cea mai valoroasa sectiune pentru luarea deciziilor. In tab-ul "Compromisuri", am simulat cum variaza profitul mediu si riscul in functie de probabilitatea de succes ($p$).

Graficul "Tradeoff" demonstreaza ca:

1. **Relatia nu este liniara:** O crestere mica a fiabilitatii (ex: de la 0.98 la 0.99) poate reduce drastic pierderile din Churn, avand un impact disproportionat de mare asupra profitului.
    
2. **Costul Churn-ului:** Tabelul de descompunere arata adesea ca, desi penalitatile SLA sunt frecvente, costul lor este mic comparativ cu pierderea unui singur client ($C_{churn}$).
    
3. **Optimizare:** Exista un punct dincolo de care costul imbunatatirii infrastructurii (pentru a creste $p$) ar putea depasi beneficiul marginal, dar in simularea noastra, cresterea lui $p$ este aproape intotdeauna profitabila datorita evitarii churn-ului.
    

![[ex_11_3.png]](imagini/ex_11_3.png)
**Concluzie Economica:**

Performanta tehnica (stabilitatea serverelor si viteza) influenteaza direct profitul. Riscul operational (zilele cu pierdere) scade semnificativ pe masura ce sistemul devine mai robust.

### 12. Vizualizare statistica
##### Cerinta:
a. Histograme pentru $T$ și profit.

b. Boxplot-uri pentru $T$ condiționat de succes/eșec și pentru scenarii diferite.

c. Interpretați mediană, IQR, outlieri.
##### Rezolvare:

Simularea doua scenarii:

**a) Histograme pentru T si Profit:**

Histogramele ne permit sa vedem forma distributiei.

- **Histograma Timpului ($T$):** Arata frecventa duratelor. De obicei, aceasta este asimetrica la dreapta (coada lunga), indicand ca majoritatea utilizatorilor au o experienta rapida, dar exista o minoritate care asteapta mult.
    
- **Histograma Profitului:** Aceasta are adesea o forma **bimodala** (doua "cocoase") sau foarte neregulata.
    
    - Un grup de valori este concentrat in zona pozitiva (succesele, unde $Profit \approx Reward$).
        
    - Un alt grup este in zona negativa (esecurile si churn-ul, unde $Profit = -Cost$).
        

![[ex_12_1.png]](imagini/ex_12_1.png)

**b) Boxplot-uri conditionate:**

Boxplot-ul (diagrama cutie-cu-mustati) este cel mai puternic instrument pentru compararea grupurilor.

In aplicatie, am generat boxplot-uri pentru $T$ conditionate de rezultatul cererii ("Succes" vs "Esec").

- **Observatie critica:** Boxplot-ul pentru "Esec" este de obicei pozitionat mult mai sus pe axa Y decat cel pentru "Succes".
    
- **Explicatie:** Un esec definitiu apare, de cele mai multe ori, dupa epuizarea tuturor celor $N_{max}$ incercari. Astfel, utilizatorii care esueaza sunt penalizati de doua ori: nu primesc serviciul si au asteptat cel mai mult timp posibil.
    

![[ex_12_2.png]](imagini/ex_12_2.png)

**c) Interpretarea indicatorilor statistici (Mediana, IQR, Outlieri):**

![[ex_12_3.png]](imagini/ex_12_3.png)
### 13. Analiza de sinteza
##### Cerinta:
În raport cu problema modelată, comentați:

a. Rolul probabilității empirice
b. Ce informații aduc condiționările
c. Utilitatea inegalităților probabilistice
d. Legătura dintre performanța tehnică și impactul economic
e. Ce parametri influențează cel mai mult rezultatele finale și ce ați modifica pentru îmbunătățirea sistemului.
##### Rezolvare:


**a) Rolul probabilitatii empirice:**

In tab-ul "Probabilitate Empirica", graficul de convergenta demonstreaza vizual **Legea Numerelor Mari**.

- Linia albastra fluctuanta (probabilitatea empirica calculata pe masura ce adaugam simulari) tinde sa se stabilizeze pe linia rosie orizontala (probabilitatea teoretica).
    
- **Concluzie:** Intr-un sistem real complex, unde formulele teoretice exacte sunt greu de dedus, putem avea incredere in simularile pe esantioane mari pentru a estima parametrii sistemului.
    

![[ex_13_1.png]](imagini/ex_13_1.png)

**b) Ce informatii aduc conditionarile:**

Conditionarile ne ajuta sa intelegem nuantele din spatele mediilor globale. Tabelul generat arata diferente clare:

- $E(T | Succes)$ vs $E(T | Esec)$: Vedem ca esecurile costa mai mult timp.
    
- $P(A | N=1)$ vs $P(A)$: Vedem ca cererile rezolvate din prima sunt garantate succese, in timp ce cele care ajung la $N_{max}$ au o rata de succes mult mai mica (sau zero).
    
- **Concluzie:** Analiza conditionata ne permite sa identificam segmentele de utilizatori cu probleme (cei care asteapta mult si tot nu primesc raspuns).
    

**c) Utilitatea inegalitatilor probabilistice:**

Tabelul din aplicatie verifica numeric limitele Markov si Cebisev.

- Chiar daca distributia timpilor este Exponentiala (sau Gamma), inegalitatile raman valabile.
    
- **Concluzie:** Acestea sunt instrumente esentiale pentru garantarea SLA. Putem promite clientilor ca "in cel mult 5% din cazuri timpul va depasi X secunde", chiar daca nu cunoastem exact distributia traficului din acea zi, bazandu-ne doar pe monitorizarea mediei si a variantei.


**d) Legatura dintre performanta tehnica si impactul economic:**

Graficul din acest tab arata o corelatie pozitiva directa intre probabilitatea de succes tehnica ($p$) si profitul mediu.

- Panta curbei ne spune cat de sensibili sunt banii la tehnologie. O panta abrupta inseamna ca o mica imbunatatire tehnica aduce un castig financiar major.
    
- **Concluzie:** Performanta tehnicaeste un  de maximizare a valorii economice. Reducerea erorilor si a latentei se traduce direct in reducerea penalitatilor si a churn-ului.
    

![[ex_13_2.png]](imagini/ex_13_2.png)

**e) Parametrii cu cea mai mare influenta (Analiza de Sensibilitate):**

Graficul "Tornado" (bar chart orizontal) compara impactul modificarii cu un procent fix a diferitilor parametri.

- **Rezultatul tipic:** Observam ca **Rata de Churn** si **Probabilitatea de Succes ($p$)** au cel mai mare impact asupra profitului (barele cele mai lungi). Penalitatea SLA are adesea un impact mai mic.
    
- **Ce as modifica pentru imbunatatirea sistemului:**
    
    1. **Prioritate Zero:** Reducerea Churn-ului. Deoarece pierderea unui client este extrem de costisitoare ($C_{churn} \gg G$), orice efort de a pastra utilizatorii (chiar si prin compensatii financiare) merita.
        
    2. **Stabilitatea (Marirea lui $p$):** Investitia in servere mai bune se amortizeaza rapid prin numarul crescut de tranzactii reusite.
        
    3. **Viteza (Latenta):** Este importanta pentru experienta, dar financiar este secundara fata de disponibilitate (succes/esec), atata timp cat penalitatile SLA nu sunt draconice.
        
![[ex_13_3.png]](imagini/ex_13_3.png)