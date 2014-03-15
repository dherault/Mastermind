
'MASTERMIND
'-----------------------------------
'ENSAM 2013
'Alexandre Callemeyn, David Hérault
'-----------------------------------
'Open source : GNU General Public License (GNU GPL)

Module CodeBreaker


    'Initialise la liste des combinaisons possibles
    Sub iniLcombi(ByRef Lcombi(,) As Integer, ByVal dimCode As Integer, ByVal nbCol As Integer)

        Dim temp(dimCode - 1) As Integer 'sorte de transposée d'une ligne de lcombi : temp(a,b,c,d)
        Dim i, j, k As Integer

        For j = 1 To dimCode 'pour chaque colonne en partant de la dernière

            For i = 0 To dimCode - 1 'on initialise temp à 1 (a=b=c=d=1)
                temp(i) = 1
            Next

            For i = 0 To nbCol ^ dimCode - 1 'pour chaque ligne

                Lcombi(i, dimCode - j) = temp(j - 1)

                temp(0) = temp(0) + 1 'on incremente temp(0) alias a à chaque tour
                For k = 0 To dimCode - 2 'on parcourt temp sauf la dernière ligne : temp(a,b,c,d)
                    If temp(k) = nbCol + 1 Then 'a=7 -> a=1 et b=b+1
                        temp(k) = 1             'b=7 -> b=1 et c=c+1 etc
                        temp(k + 1) = temp(k + 1) + 1
                    End If
                Next k

            Next i
        Next j

    End Sub

    'Donne le premier coup
    Sub FirstGuess(ByRef combi() As Integer, ByVal dimcode As Integer, ByVal nbcol As Integer)

        Dim a, b, i, moitmoit As Integer
        Randomize()

        moitmoit = CInt(dimcode / 2)

        a = CInt(Int(nbcol * Rnd()) + 1)
        Do
            b = CInt(Int(nbcol * Rnd()) + 1)
        Loop While a = b

        For i = 0 To moitmoit - 1
            combi(i) = a
        Next
        For i = moitmoit To dimcode - 1
            combi(i) = b
        Next

    End Sub

    'Compte le nombre le lignes non-vides d'un tableau
    Function CountLines(ByVal List(,) As Integer, ByVal NBCOL As Integer, ByVal DIMCODE As Integer) As Integer

        Dim c As Integer = 0
        Dim i As Integer

        For i = 0 To NBCOL ^ DIMCODE - 1

            If List(i, 0) <> 0 Then
                c = c + 1
            End If

        Next

        Return c
    End Function

    'Enleve les combinaisons impossibles de Di
    Sub RemovePos(ByRef mdList(,) As Integer, ByVal nbcol As Integer, ByVal dimcode As Integer, ByVal mcode() As Integer, ByVal mcombi() As Integer)

        Dim refNoire As Integer = Noire(mcode, mcombi, dimcode)
        Dim refBlanche As Integer = Blanche(mcode, mcombi, dimcode)
        Dim transfertligne(dimcode) As Integer
        Dim i, j As Integer

        For i = 0 To nbcol ^ dimcode - 1

            If mdList(i, 0) = 0 Then 'Si la ligne est vide on passe à la suivante
                Continue For
            End If

            For j = 0 To dimcode - 1 'dans le cas contraire on stoque la ligne dans une variable de transfert
                transfertligne(j) = mdList(i, j)
            Next j

            'puis si n/b(combi = combijouee, code = Di) <> nref/bref on supprime la ligne
            If Noire(transfertligne, mcombi, dimcode) <> refNoire Or Blanche(transfertligne, mcombi, dimcode) <> refBlanche Then
                For j = 0 To dimcode - 1
                    mdList(i, j) = 0
                Next j
            End If

        Next i

    End Sub

    'Compte le nombre de lignes restantes si n/b(combi = Si, code = Di) <> n/b => suppr Di
    Function CountRestantes(ByVal dynList(,) As Integer, ByVal mCombi() As Integer, ByVal NBCOL As Integer, ByVal DIMCODE As Integer, ByVal n As Integer, ByVal b As Integer) As Integer

        Dim lignesrestantes As Integer = CountLines(dynList, NBCOL, DIMCODE)
        Dim transfertligne(DIMCODE) As Integer
        Dim i, j As Integer

        For i = 0 To NBCOL ^ DIMCODE - 1
            If dynList(i, 0) = 0 Then
                Continue For
            End If

            For j = 0 To DIMCODE - 1
                transfertligne(j) = dynList(i, j)
            Next
            If Noire(transfertligne, mCombi, DIMCODE) <> n Or Blanche(transfertligne, mCombi, DIMCODE) <> b Then
                lignesrestantes = lignesrestantes - 1
            End If
        Next

        Return lignesrestantes
    End Function

    'Gère le GHI du jeu inversé ('faire jouer l'ordinateur')
    Sub reverseGame(ByVal DIMCODE As Integer, ByVal NBCOL As Integer, ByVal NBCOUPS As Integer)

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
        'Const DIMCODE As Integer = 5       'la taille du code choisie
        'Const NBCOL As Integer = 6         'le nb de couleurs choisies
        'Const NBCOUPS As Integer = 12      'le nb de coups choisis

        Dim input As String                'le code du joueur
        Dim coup As Integer                'le coup joué
        Dim codeenclair As String
        Dim check As Boolean
        Dim findepartie As Boolean
        Dim maxi, mini, posmini As Integer 'cf algo
        Dim i, j, k, n, b, c, nvraie, bvraie As Integer
        Dim posrestantes As Integer = NBCOL ^ DIMCODE - 1

        Dim mdList(NBCOL ^ DIMCODE - 1, DIMCODE - 1) As Integer 'D : la liste des combinaisons DYNAMIQUE |  6^4=1296
        Dim msList(NBCOL ^ DIMCODE - 1, DIMCODE - 1) As Integer 'S : la liste des combinaisons STATIQUE  
        Dim mCode(DIMCODE - 1) As Integer                       'le code
        Dim mCombi(DIMCODE - 1) As Integer                      'la proposition de l'ordi
        Dim mCombiTemp(DIMCODE - 1) As Integer                  'une matrice de transfert
        Dim mCouleurs(NBCOLMAX, 1) As String                    'la matrice des couleurs
        Dim mJeu(NBCOUPS - 1, DIMCODE - 1) As Integer           'la matrice de jeu
        Dim mClefs(NBCOUPS - 1, 1) As Integer                   'la matrice des clefs
        Dim mRest(NBCOL ^ DIMCODE - 1) As Integer               'le nombre de possiblités restantes

        System.Console.Clear()

        coup = 0
        findepartie = False

        'ini couleurs
        IniCol(mCouleurs)

        'initialisation des listes de possibilités
        iniLcombi(mdList, DIMCODE, NBCOL)
        iniLcombi(msList, DIMCODE, NBCOL)

        'On demande le code secret, on le vérifie puis on l'implémente
        System.Console.WriteLine(vbCrLf & " Vous allez faire jouer l'ordinateur, les paramètres du     jeu sont ceux du menu et les doublons dans le code sont    autorisés.")
        System.Console.WriteLine(" Ne vous méprenez pas il est très probable qu'il gagne ;)" & vbCrLf)
        System.Console.WriteLine(" ATTENTION : Une taille du code supérieure à 4 entraine                 des temps de calcul importants." & vbCrLf & vbCrLf)
        System.Console.WriteLine(" Veuillez entrer un code de " & NBCOL & " couleurs parmi les couleurs   suivantes :" & vbCrLf)
        For i = 1 To NBCOL
            System.Console.WriteLine("  " & mCouleurs(i, 0) & " : " & mCouleurs(i, 1))
        Next
        System.Console.WriteLine(vbCrLf & " Exemple : vjrb")
        System.Console.WriteLine(" Quitter : x" & vbCrLf)

        input = System.Console.ReadLine()

        If input = "x" Then 'si l'utilisateur souhaite abandonner
            Exit Sub
        End If

        Do While CheckInput(input, DIMCODE, mCouleurs, NBCOL) = False
            System.Console.Clear()
            If input <> "x" Then
                System.Console.WriteLine(vbCrLf & " Votre proposition a été rejetée." & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf)
                System.Console.WriteLine(" Veuillez entrer un code de 4 couleurs parmi les couleurs   suivantes :" & vbCrLf)
                For i = 1 To NBCOL
                    System.Console.WriteLine("  " & mCouleurs(i, 0) & " : " & mCouleurs(i, 1))
                Next
                System.Console.WriteLine(vbCrLf & " Exemple : vjrb")
                System.Console.WriteLine(" Quitter : x" & vbCrLf)
            End If
            input = System.Console.ReadLine()
            If input = "x" Then
                Exit Sub
            End If
        Loop


        codeenclair = input
        For i = 0 To DIMCODE - 1
            mCode(i) = Conversion(Mid(input, i + 1, 1), mCouleurs, NBCOL) 'on extrait une lettre dans input grace a Mid (ex : J dans NJRV)
        Next

        'on génère et implémente le 1er coup dans mCombi et mJeu
        FirstGuess(mCombi, DIMCODE, NBCOL)
        For j = 0 To DIMCODE - 1
            mJeu(coup, j) = mCombi(j)
        Next

        System.Console.WriteLine(vbCrLf & " La partie commence !")
        Wait(1110)



        Do
            System.Console.Clear()

            'Calcul de noire
            nvraie = Noire(mCode, mCombi, DIMCODE)
            If nvraie = DIMCODE Then 'corrige un beug (cas de figure)
                mClefs(coup, 0) = nvraie
                mClefs(coup, 1) = 0
                'coup = coup + 1
                GoTo lblfin
            End If

            'Calcul de blanche
            bvraie = Blanche(mCode, mCombi, DIMCODE)

            'on affiche le plateau 
            Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)

            'On demande la clef noire à l'utilisateur
            System.Console.WriteLine(" Votre code : " & codeenclair & vbCrLf)
            System.Console.WriteLine(" Coup " & coup + 1 & " : il reste " & posrestantes & " possibilités à l'ordinateur." & vbCrLf)
            System.Console.WriteLine(" Quelle est la clef noire ?")
            input = System.Console.ReadLine()
            If input = "x" Then
                Exit Sub
            End If

            'On les vérifie
            check = False
            If CheckInterval(input, 0, DIMCODE) = True Then
                If CInt(input) = nvraie Then
                    check = True
                End If
            End If

            Do While check = False
                System.Console.Clear()
                Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)
                System.Console.WriteLine(" Votre code : " & codeenclair & vbCrLf)
                System.Console.WriteLine(" Coup " & coup + 1 & " : il reste " & posrestantes & " possibilités à l'ordinateur." & vbCrLf)
                System.Console.WriteLine(" Tricher c'est mal, ne pas savoir compter c'est pire !")
                System.Console.WriteLine(" Quelle est la clef noire ?")
                input = System.Console.ReadLine()
                If input = "x" Then
                    Exit Sub
                End If
                If CheckInterval(input, 0, DIMCODE) = True Then
                    If CInt(input) = nvraie Then
                        check = True
                    End If
                End If
            Loop

            'Calcul de noire
            mClefs(coup, 0) = nvraie

            'on affiche le plateau 
            System.Console.Clear()
            Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)

            'On demande la clef blanche à l'utilisateur
            System.Console.WriteLine(" Votre code : " & codeenclair & vbCrLf)
            System.Console.WriteLine(" Coup " & coup + 1 & " : il reste " & posrestantes & " possibilités à l'ordinateur." & vbCrLf)
            System.Console.WriteLine(" Quelle est la clef blanche ?")
            input = System.Console.ReadLine()
            If input = "x" Then
                Exit Sub
            End If

            'On les vérifie
            check = False
            If CheckInterval(input, 0, DIMCODE) = True Then
                If CInt(input) = bvraie Then
                    check = True
                End If
            End If

            Do While check = False
                System.Console.Clear()
                Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)
                System.Console.WriteLine(" Votre code : " & codeenclair & vbCrLf)
                System.Console.WriteLine(" Coup " & coup + 1 & " : il reste " & posrestantes & " possibilités à l'ordinateur." & vbCrLf)
                System.Console.WriteLine(" Tricher c'est mal, ne pas savoir compter c'est pire !")
                System.Console.WriteLine(" Quelle est la clef blanche ?")
                input = System.Console.ReadLine()
                If input = "x" Then
                    Exit Sub
                End If
                If CheckInterval(input, 0, DIMCODE) = True Then
                    If CInt(input) = bvraie Then
                        check = True
                    End If
                End If
            Loop

            'Calcul de blanche
            mClefs(coup, 1) = bvraie

            coup = coup + 1
            k = 0

            'On retire les combinaisons impossibles de D
            RemovePos(mdList, NBCOL, DIMCODE, mCode, mCombi)

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

                        'on stocke le plus grand c dans maxi
                        If c > maxi Then
                            maxi = c
                        End If

                    Next b
                Next n

                'chaque combinaison possede alors son maximum de possibilités restantes si elle est jouée
                mRest(i) = maxi

                If Int(100 * i / (NBCOL ^ DIMCODE)) = k Then
                    k = k + 1
                    System.Console.Clear()
                    Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, NBCOL)
                    System.Console.WriteLine(" Votre code : " & codeenclair & vbCrLf)
                    System.Console.WriteLine(" Coup " & coup + 1 & " : il reste " & posrestantes & " possibilités à l'ordinateur." & vbCrLf)
                    System.Console.WriteLine(" Quelle est la clef blanche ?")
                    System.Console.WriteLine(bvraie & vbCrLf)
                    System.Console.WriteLine("Calcul... " & k - 1 & "%")
                End If

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
                            mJeu(coup, j) = mCombi(j)
                        Next
                        mClefs(coup, 0) = Noire(mCode, mCombi, DIMCODE)
                        mClefs(coup, 1) = Blanche(mCode, mCombi, DIMCODE)
                        findepartie = True 'Bon pour ca on triche, mais pas vraiment en fait, puisque l'algo gagne tjrs (cf. Wikipedia ou un bon cours de maths)
                        Exit For
                    End If
                Next i
                'Sinon on joue le minimum du maximum des possibilités restantes
            Else
                For j = 0 To DIMCODE - 1
                    mCombi(j) = msList(posmini, j)
                    mJeu(coup, j) = mCombi(j)
                Next
            End If

            posrestantes = CountLines(mdList, NBCOL, DIMCODE) - 1

        Loop While findepartie = False

        System.Console.Clear()

lblfin: Display(NBCOUPS, DIMCODE, DIMCODEMIN, mJeu, mClefs, coup + 1, mCouleurs, NBCOL)

        System.Console.WriteLine("Owned")
        System.Console.ReadLine()

    End Sub


End Module
