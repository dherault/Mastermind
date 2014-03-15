
'MASTERMIND
'-----------------------------------
'ENSAM 2013
'Alexandre Callemeyn, David Hérault
'-----------------------------------
'Open source : GNU General Public License (GNU GPL)


'LANCER CE MODULE POUR VOIR JOUER L'ORDINATEUR CONTRE LUI MEME EN BOUCLE

Module OrdiVsOrdi

    Sub Main()

        'Ceci est le code initial de "l'intelligence atificielle"

        'Algorithme :
        '----------------------------------------------------------------------------------------------------------------------------------------------------
        'Five-guess algorithm (source : en.wikipedia)
        'In 1977, Donald Knuth demonstrated that the codebreaker can solve the pattern in five moves or fewer, 
        'using an algorithm that progressively reduced the number of possible patterns.

        '0 Create a set S of remaining possibilities (at this point there are 1296). The first guess is aabb.
        '1 Remove all possibilities from S that would not give the same score of colored and white pegs if they were the answer.
        '2 For each possible guess (not necessarily in S) calculate how many possibilities from S would be eliminated for each possible colored/white score. 
        '3 The score of the guess is the least of such values. 
        '4 Play the guess with the highest score (minimax).
        '5 Go back to step 2 until you have got it right.

        'REMARQUE : n(code,combi) = n(combi,code), l'idée de l'algo est d'exploiter cette remarque (j'ai mis 2 mois a comprendre l'algo...)
        '0 Creer 2 listes S (statique) et D (dynamique) de toutes les combinaisons possibles
        '1 le premier coup est combijouee = aabb => nref et bref
        '2 si n/b(combi = combijouee, code = Di) <> nref/bref alors supprimer Di car Di ne peux pas etre le code sinon on obtiendrai n = nref b = bref
        '3 Considerer pour chaque Si restant le nombre rest(n,b)(i) de Di restants si (n/b(combi = Si, code = Di) <> n/b => suppr Di) pour tout n/b.
        '4 Prendre alors Maxirestant(i) = Max(rest(n,b)(i))
        '5 Jouer combi = Min(Maxirestant(i)) pour qu'il ne reste au minimum que le maximum de combinaisons possibles dans Di.

        'Déclarations :
        Const DIMCODEMIN As Integer = 4    'la taille mini du code
        Const NBCOLMAX As Integer = 10     'le nb de couleurs maxi
        Const DIMCODE As Integer = 4       'la taille du code choisie
        Const NBCOL As Integer = 6         'le nb de couleurs choisies
        Const NBCOUPS As Integer = 12      'le nb de coups choisis
        Const FIN As Integer = 111160      'le nombre de parties jouées

        Dim partie As Integer = 1          'compteur de parties jouées
        Dim moy As Single = 0              'moyenne des coups nécessaires à une partie
        Dim coup As Integer                'le compteur du coup joué
        Dim findepartie As Boolean
        Dim maxi, mini, posmini As Integer 'cf algo
        Dim i, j, n, b, c As Integer


        Do 'Loop While partie <> FIN

            Dim mdList(NBCOL ^ DIMCODE - 1, DIMCODE - 1) As Integer 'D : la liste des combinaisons DYNAMIQUE
            Dim msList(NBCOL ^ DIMCODE - 1, DIMCODE - 1) As Integer 'S : la liste des combinaisons STATIQUE  |  6^4=1296
            Dim mCode(DIMCODE - 1) As Integer                       'le code
            Dim mCombi(DIMCODE - 1) As Integer                      'la proposition de l'ordi
            Dim mCombiTemp(DIMCODE - 1) As Integer                  'une matrice de transfert
            Dim mCouleurs(NBCOLMAX, 1) As String                    'la matrice des couleurs
            Dim mJeu(NBCOUPS - 1, DIMCODE - 1) As Integer           'la matrice de jeu
            Dim mClefs(NBCOUPS - 1, 1) As Integer                   'la matrice des clefs
            Dim mRest(NBCOL ^ DIMCODE - 1) As Integer               'le nombre de possiblités restantes

            coup = 1
            findepartie = False

            'ini couleurs
            IniCol(mCouleurs)

            'initialisation des listes de possibilités
            iniLcombi(mdList, DIMCODE, NBCOL)
            iniLcombi(msList, DIMCODE, NBCOL)

            'génération du code secret
            IniCode(mCode, DIMCODE, NBCOL, False)

            'on implémente le 1er coup dans mCombi
            FirstGuess(mCombi, DIMCODE, NBCOL)
            For j = 0 To DIMCODE - 1
                mJeu(coup - 1, j) = mCombi(j)
            Next

            'Calcul de noir
            mClefs(coup - 1, 0) = Noire(mCode, mCombi, DIMCODE)

            'Calcul de blanc
            mClefs(coup - 1, 1) = Blanche(mCode, mCombi, DIMCODE)

            'on affiche le plateau 
            Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)


            Do 'Loop While findepartie = False

                coup = coup + 1

                'On retire les combinaisons impossibles de D
                RemovePos(mdList, NBCOL, DIMCODE, mCode, mCombi)

                System.Console.WriteLine("Coup " & coup - 1 & " : il reste " & CountLines(mdList, NBCOL, DIMCODE) & " lignes a D")

                'Pour chaque combinaison (possible ou pas, donc de S)
                For i = 0 To NBCOL ^ DIMCODE - 1

                    'on stocque chaque ligne de S dans une matrice de transfert
                    For j = 0 To DIMCODE - 1
                        mCombiTemp(j) = msList(i, j)
                    Next j

                    maxi = 0

                    'pour chaque clef noire possible
                    For n = 0 To DIMCODE
                        'Pour chaque clef blanche possible
                        For b = 0 To DIMCODE - n

                            'Pour ne considerer que (3,0) et (4,0) si n>=3 (pas (3,1) qui n'existe pas)
                            If n >= 3 And b > 0 Then
                                Continue For
                            End If

                            'Compte le nombre de lignes restantes quand ((noire ou blanche(combi = Si, code = Di) <> n ou b => suppr Di))
                            c = CountRestantes(mdList, mCombiTemp, NBCOL, DIMCODE, n, b)

                            'on stoque le plus grand c dans maxi
                            If c > maxi Then
                                maxi = c
                            End If

                        Next b
                    Next n

                    'chaque combinaison possede alors son maximum de possibilités restantes si elle est jouée
                    mRest(i) = maxi

                Next i

                'on va maintenant chercher le minimum de mRest
                mini = NBCOL ^ DIMCODE
                posmini = 0

                'pour chaque mRest(i)
                For i = 0 To NBCOL ^ DIMCODE - 1
                    'on note la position du minimum
                    If mRest(i) < mini Then
                        mini = mRest(i)
                        posmini = i
                    End If
                Next

                'Si il ne reste plus qu'une ligne à D alors c'est la solution, on la cherche dans D et on la joue
                If CountLines(mdList, NBCOL, DIMCODE) = 1 Then
                    For i = 0 To NBCOL ^ DIMCODE
                        If mdList(i, 0) <> 0 Then
                            For j = 0 To DIMCODE - 1
                                mCombi(j) = mdList(i, j)
                                mJeu(coup - 1, j) = mCombi(j)
                            Next
                            Exit For
                        End If
                    Next i
                    'Sinon on joue le minimum du maximum des possibilités restantes
                Else
                    For j = 0 To DIMCODE - 1
                        mCombi(j) = msList(posmini, j)
                        mJeu(coup - 1, j) = mCombi(j)
                    Next
                End If

                'Calcul de noir
                mClefs(coup - 1, 0) = Noire(mCode, mCombi, DIMCODE)

                'Calcul de blanc
                mClefs(coup - 1, 1) = Blanche(mCode, mCombi, DIMCODE)

                'on affiche le plateau
                Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)

                'On vérifie si on a gagné (de maniere honnete avec la clef noire car on pourrait aussi le faire lorsque qu'il ne reste plus qu'une combinaison à D)
                If mClefs(coup - 1, 0) = DIMCODE Or coup = NBCOUPS Then
                    findepartie = True
                End If

            Loop While findepartie = False

            'Stats
            moy = (moy * (partie - 1) + coup) / partie
            System.Console.WriteLine("Fin de la partie " & partie & " - " & coup & " coups - en moyenne " & moy & " coups")
            partie = partie + 1

        Loop While partie <> FIN

        System.Console.WriteLine("En moyenne sur " & partie & " parties : " & moy & " coups.")
        System.Console.ReadLine()

    End Sub

End Module
