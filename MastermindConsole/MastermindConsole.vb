
'MASTERMIND
'-----------------------------------
'ENSAM 2013
'Alexandre Callemeyn, David Hérault
'-----------------------------------
'Open source : GNU General Public License (GNU GPL)

'Ces lignes sont optionnelles et implémentées par default
Option Explicit On 'Declaration des variables obligatoire
Imports System     'Ajout des librairies au projet
Imports System.IO
Imports System.Text
Imports System.Math 'Pas par default : usitée pour Round()


Module MastermindConsole

    'Consignes :
    '------------------------------------------------------------------------------------------------------------------------------------
    'A FAIRE :   • (FINI) calcul de la clé blanche (PHASE 1)
    '            • (FINI) vérification du choix des parametres de base (PHASE 1) 
    '            • (FINI) rendre modifiable le nombre de couleurs composant le code secret (PHASE 2)
    '            • (FINI) rendre possible d'interdire les doublons de couleurs (PHASE 2)
    'OPTIONNEL : • (FINI) implémenter une intelligence artificielle (l’ordinateur doit deviner un code choisi par l’utilisateur)
    '            • (FINI) implémenter une intelligence artificielle capable de deviner n’importe quel code en 5 coups au maximum(règles de base)
    '            • (PAS VRAIMENT FAIS) permettre à tout moment d’enregistrer/charger l’état de la partie dans/depuis un fichier
    '------------------------------------------------------------------------------------------------------------------------------------

    Sub Main()

        Const DIMCODEMIN As Integer = 4    'la taille mini du code
        Const DIMCODEMAX As Integer = 6    'la taille maxi du code
        Const NBCOLMIN As Integer = 6      'le nb de couleurs mini 
        Const NBCOLMAX As Integer = 10     'le nb de couleurs maxi
        Const COUPSMIN As Integer = 12     'le nb de coups mini
        Const COUPSMAX As Integer = 21     'le nb de coups maxi
        Const DIMMENU As Integer = 13      'le nb de choix possibles dans le menu
        Const JUSTI2 As String = "      "  'les justifications du menu
        Const JUSTI3 As String = "    "
        Const JUSTI1 As String = "         "
        Const STATSPATH As String = "\Statistiques.txt"        'le nom du fichier de sauvegarde des statistiques
        Const GAMESPATH As String = "\PartiesEnregistrées.txt" 'le nom du fichier de sauvegarde des parties


        Dim dimCode As Integer = 4         'la taille du code choisie
        Dim nbCol As Integer = 6           'le nb de couleurs choisies
        Dim nbCoups As Integer = 12        'le nb de coups choisis
        Dim coup As Integer                'le compteur du coup joué
        Dim input As String                'la proposition "brute" du joueur
        Dim doublons As Boolean = False    'possibilité de doublons
        Dim posCursor As Integer = 1       'la position du curseur dans le menu
        Dim getKey As ConsoleKeyInfo       'une classe systeme pour lire les touches
        Dim mCursor(DIMMENU - 1) As String 'place le curseur en face du choix de l'utilisateur
        Dim victoire As Boolean            'indicateur de victoire
        Dim chrono As Integer              'le chronometre de la partie
        Dim colorFont As Single = 15       'la couleur des ecritures = blanc
        Dim colorBack As Single = 0        'la couleur du fond = noire
        Dim sessionCount As Integer = 1    'le nombre de partie jouée dans une session
        Dim transferKey As ConsoleKey      'une 'touche' de transfert pour le menu
        Dim liveStats As New Stats         'une classe de valeurs et string pour les statistiques
        Dim nbEnregistrees As Integer      'le nombre de parties enregistrées
        Dim temp(2) As String              'la durée (minutes,secondes) de la partie
        Dim i As Integer                   'ce bon vieux i qui se tape le sale boulot

        'Initialisation de la console
        System.Console.WindowHeight = 39
        System.Console.BufferHeight = 39
        System.Console.WindowWidth = 59
        System.Console.BufferWidth = 59
        System.Console.BackgroundColor = colorBack
        System.Console.ForegroundColor = colorFont

        'ini de la sauvegarde et des stats
        iniSave(STATSPATH, GAMESPATH) 'initialisation des fichiers
        liveStats.iniStats(STATSPATH) 'initialisation des statistiques
        nbEnregistrees = countChar("#", GAMESPATH)


        Do 'La boucle qui englobe tout le programme pour refaire une partie, Loop sans condition

            System.Console.CursorVisible = False 'on cache le curseur '_' lorsqu'on affiche le menu
            System.Console.Clear()

lblMenu:    Do 'Debut du menu 

                Console.Title = "Mastermind"
                System.Console.WriteLine(vbCrLf & "        ╔════════════════════════════════════════╗")
                System.Console.WriteLine("        ║            Mastermind v5.0             ║")
                System.Console.WriteLine("        ║          Alexandre Callemeyn           ║")
                System.Console.WriteLine("        ║             David Hérault              ║")
                System.Console.WriteLine("        ╚════════════════════════════════════════╝" + vbCrLf + vbCrLf)

                'On cree le curseur en fonction de la position du menu
                'en implementant la forme de base à tous les niveaux
                For i = 0 To DIMMENU - 1
                    If i = posCursor - 1 Then
                        mCursor(i) = "► "
                    Else
                        mCursor(i) = "  "
                    End If
                Next
                'puis en traitants ceux possedants un selecteur
                If posCursor = 2 Then
                    mCursor(1) = "◄  " & dimCode & " ►"
                Else
                    mCursor(1) = "   " & dimCode
                End If
                If posCursor = 3 Then
                    If nbCol < 10 Then : mCursor(2) = "◄  " & nbCol & " ►" : Else : mCursor(2) = "◄ " & nbCol & " ►" : End If
                Else
                    If nbCol < 10 Then : mCursor(2) = "   " & nbCol : Else : mCursor(2) = "  " & nbCol : End If
                End If
                If posCursor = 4 Then
                    mCursor(3) = "◄ " & nbCoups & " ►"
                Else
                    mCursor(3) = "  " & nbCoups
                End If
                If posCursor = 5 Then
                    If doublons = True Then : mCursor(4) = "◄ Autorisés ►" : Else : mCursor(4) = "◄ Interdits ►" : End If
                Else
                    If doublons = True Then : mCursor(4) = "  Autorisés  " : Else : mCursor(4) = "  Interdits  " : End If
                End If
                If posCursor = 10 Then : mCursor(9) = "◄ Texte ►" : Else : mCursor(9) = "  Texte" : End If
                If posCursor = 11 Then : mCursor(10) = "◄ Fond  ►" : Else : mCursor(10) = "  Fond" : End If

                'on affiche le menu

                System.Console.WriteLine(JUSTI1 & mCursor(0) & "Nouvelle Partie")
                System.Console.WriteLine(JUSTI1 & JUSTI2 & "Taille du code  " & mCursor(1))
                System.Console.WriteLine(JUSTI1 & JUSTI2 & "Nb de couleurs  " & mCursor(2))
                System.Console.WriteLine(JUSTI1 & JUSTI2 & "Nombre de coups " & mCursor(3))
                System.Console.WriteLine(JUSTI1 & JUSTI2 & "Doublons " & mCursor(4) & vbCrLf)
                System.Console.WriteLine(JUSTI1 & mCursor(5) & "Faire jouer l'ordinateur" & vbCrLf)
                System.Console.WriteLine(JUSTI1 & mCursor(6) & "Parties enregistrées (" & CStr(nbEnregistrees) & ")")
                System.Console.WriteLine(JUSTI1 & JUSTI3 & mCursor(7) & "Supprimer les parties enregistrées")
                System.Console.WriteLine(JUSTI1 & JUSTI3 & mCursor(8) & "Réinitialiser les statistiques" & vbCrLf)
                System.Console.WriteLine(JUSTI1 & "  Paramètres d'affichage :")
                System.Console.WriteLine(JUSTI1 & JUSTI3 & mCursor(9))
                System.Console.WriteLine(JUSTI1 & JUSTI3 & mCursor(10) & vbCrLf)
                System.Console.WriteLine(JUSTI1 & mCursor(11) & "Règles" & vbCrLf)
                System.Console.WriteLine(JUSTI1 & mCursor(12) & "Quitter")

                'puis les stats
                System.Console.WriteLine(vbCrLf & "───────────────────────────────────────────────────────────")
                System.Console.WriteLine(JUSTI1 & "STATISTIQUES des parties classiques" & vbCrLf)
                System.Console.WriteLine(JUSTI1 & liveStats.line1)
                System.Console.WriteLine(JUSTI1 & liveStats.line2)
                System.Console.WriteLine(JUSTI1 & liveStats.line3)
                System.Console.WriteLine(JUSTI1 & liveStats.line4)
                System.Console.WriteLine(JUSTI1 & liveStats.line5)

                'inscrit la prochaine touche enfoncée dans getkey
                getKey = Console.ReadKey() 'Attend et lit la touche enfoncée
                transferKey = getKey.Key   'on stoque la touche enfoncée dans une variable de transfert pour ne pas sortir du menu sur un selecteur


                'on s'interesse maintenant à la touche qui vient d'etre pressee

                'si c'est haut ou bas on bouge le curseur
                If getKey.Key = ConsoleKey.UpArrow Then
                    posCursor = posCursor - 1
                ElseIf getKey.Key = ConsoleKey.DownArrow Then
                    posCursor = posCursor + 1
                End If
                'on boucle la position du curseur
                If posCursor < 1 Then
                    posCursor = DIMMENU
                ElseIf posCursor > DIMMENU Then
                    posCursor = 1
                End If

                'si elle se trouve sur un selecteur et que c'est gauche ou droite
                If posCursor = 2 Then                              'taille du code
                    If getKey.Key = ConsoleKey.LeftArrow Then
                        dimCode = dimCode - 1
                        If dimCode < DIMCODEMIN Then                    'pour boucler la valeur de la variable
                            dimCode = DIMCODEMAX
                        End If
                    ElseIf getKey.Key = ConsoleKey.RightArrow Then
                        dimCode = dimCode + 1
                        If dimCode > DIMCODEMAX Then
                            dimCode = DIMCODEMIN
                        End If
                    End If
                ElseIf posCursor = 3 Then                          'nombre de couleurs
                    If getKey.Key = ConsoleKey.LeftArrow Then
                        nbCol = nbCol - 1
                        If nbCol < NBCOLMIN Then
                            nbCol = NBCOLMAX
                        End If
                    ElseIf getKey.Key = ConsoleKey.RightArrow Then
                        nbCol = nbCol + 1
                        If nbCol > NBCOLMAX Then
                            nbCol = NBCOLMIN
                        End If
                    End If
                ElseIf posCursor = 4 Then                          'nombre de coups
                    If getKey.Key = ConsoleKey.LeftArrow Then
                        nbCoups = nbCoups - 1
                        If nbCoups < COUPSMIN Then
                            nbCoups = COUPSMAX
                        End If
                    ElseIf getKey.Key = ConsoleKey.RightArrow Then
                        nbCoups = nbCoups + 1
                        If nbCoups > COUPSMAX Then
                            nbCoups = COUPSMIN
                        End If
                    End If
                ElseIf posCursor = 5 Then                          'doublons
                    If getKey.Key = ConsoleKey.LeftArrow Or getKey.Key = ConsoleKey.RightArrow Then
                        If doublons = True Then
                            doublons = False
                        Else
                            doublons = True
                        End If
                    End If
                ElseIf posCursor = 10 Then                'Couleur du texte
                    If getKey.Key = ConsoleKey.LeftArrow Then 'si la fleche gauche est enfoncée 
                        colorFont = colorFont - 1             'alors on passe à la couleur precedante
                        If colorFont < 0 Then                 'si on depasse l'index des couleurs de la console (de 0 à 15)
                            colorFont = 15                    'on corrige le tir en bouclant la couleur
                        End If
                        If colorFont = colorBack Then         'et si les deux couleurs sont identiques (on ne voit plus rien)
                            colorFont = colorFont - 1         'on passe de nouveau la à la couleur precedante
                        End If
                        If colorFont < 0 Then                 'si on depasse cette fois ci l'index des couleurs de la console (de 0 à 15)
                            colorFont = 15                    'on corrige le tir en bouclant la couleur
                        End If
                        Console.ForegroundColor = colorFont   'on applique la nouvelle couleur
                    ElseIf getKey.Key = ConsoleKey.RightArrow Then
                        colorFont = colorFont + 1
                        If colorFont > 15 Then
                            colorFont = 0
                        End If
                        If colorFont = colorBack Then
                            colorFont = colorFont + 1
                        End If
                        If colorFont > 15 Then
                            colorFont = 0
                        End If
                        Console.ForegroundColor = colorFont
                    End If

                ElseIf posCursor = 11 Then                'Couleur du fond
                    If getKey.Key = ConsoleKey.LeftArrow Then
                        colorBack = colorBack - 1
                        If colorBack < 0 Then
                            colorBack = 15
                        End If
                        If colorBack = colorFont Then
                            colorBack = colorBack - 1
                        End If
                        If colorBack < 0 Then
                            colorBack = 15
                        End If
                        Console.BackgroundColor = colorBack
                    ElseIf getKey.Key = ConsoleKey.RightArrow Then
                        colorBack = colorBack + 1
                        If colorBack > 15 Then
                            colorBack = 0
                        End If
                        If colorBack = colorFont Then
                            colorBack = colorBack + 1
                        End If
                        If colorBack > 15 Then
                            colorBack = 0
                        End If
                        Console.BackgroundColor = colorBack
                    End If
                End If
                'on efface tout et on recommence !
                System.Console.Clear()
                'Si on se trouve sur un selecteur
                If posCursor = 2 Or posCursor = 3 Or posCursor = 4 Or posCursor = 5 Or posCursor = 10 Or posCursor = 11 Then
                    transferKey = Nothing  'passer par transfertkey ne peut plus etre 'enter' ou 'spacebar'
                End If

                'on boucle tant qu'on appuie pas sur Echap, Entrée ou Espace
            Loop While getKey.Key <> ConsoleKey.Escape And transferKey <> ConsoleKey.Enter And transferKey <> ConsoleKey.Spacebar
            'fin du menu

            'une fois que l'utilisateur a appuyé sur Echap, entree ou espace 
            If getKey.Key = ConsoleKey.Escape Then 'si l'utilisateur a choisi Echap
                Exit Sub
            End If

            'on s'interesse à la position du curseur
            Select Case posCursor
                Case 1   'Nouvelle partie'
                    System.Console.Clear()

                Case 6  'Faire jouer l'ordinateur
                    Console.Title = "Faire jouer l'ordinateur"
                    setBonJeu()
                    reverseGame(dimCode, nbCol, nbCoups)
                    System.Console.Clear()
                    GoTo lblMenu

                Case 7      'Parties enregistrées'
                    If nbEnregistrees = 0 Then : GoTo lblMenu                                        'si il n'y a aucune partie on retourne au menu
                    Else
                        visioGame(GAMESPATH, DIMCODEMIN, NBCOLMAX, COUPSMAX)                         'sinon on lance la visionneuse
                        System.Console.Clear()
                        GoTo lblMenu                                                                 'on retourne au menu
                    End If

                Case 8     'Supprimer les parties enregistrées'
                    My.Computer.FileSystem.DeleteFile(My.Application.Info.DirectoryPath & GAMESPATH)  'on supprime le fichier de sauvegade des stats (c'est un peu bourin)
                    iniSave(STATSPATH, GAMESPATH)                                                     'on en crée un nouveau
                    nbEnregistrees = 0
                    GoTo lblMenu                                                                      'on retourne au menu

                Case 9     'Réinitialiser les statistiques'
                    My.Computer.FileSystem.DeleteFile(My.Application.Info.DirectoryPath & STATSPATH) 'on supprime le fichier de sauvegade des stats (c'est un peu bourin)
                    iniSave(STATSPATH, GAMESPATH)                                                    'on en crée un nouveau
                    liveStats.iniStats(STATSPATH)                                                    'et on réinitialise les stats
                    GoTo lblMenu                                                                     'on retourne au menu     

                Case 12     'Regles'
                    setRegles()
                    GoTo lblMenu
                Case DIMMENU 'Quitter'
                    Exit Sub
                Case Else
                    GoTo lblMenu
            End Select



            'Affiche 'Bon jeu !'
            Console.Title = "Partie " & sessionCount
            setBonJeu()

            'Debut d'une partie
            coup = 0

            'création des matrices de jeu à partir des parametres
            Dim mCode(dimCode - 1) As Integer             'le code
            Dim mCombi(dimCode - 1) As Integer            'la proposition du joueur
            Dim mJeu(nbCoups - 1, dimCode - 1) As Integer 'la matrice de jeu
            Dim mClefs(nbCoups - 1, 1) As Integer         'la matrice des clefs
            Dim mCouleurs(NBCOLMAX, 1) As String          'la matrice listant les couleurs et leur lettre

            'génération du code secret
            IniCode(mCode, dimCode, nbCol, doublons)

            'initialisation de la matrice des couleurs
            IniCol(mCouleurs)

            'on affiche le plateau vide
            Display(nbCoups, dimCode, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, nbCol)

            'Debut du chronometrage
            chrono = 3600 * Date.Now.Hour + 60 * Date.Now.Minute + Date.Now.Second 'Il existe surement une methode pour le faire mais bon...


            'la boucle principale de la partie 
            Do ' Loop While turn <> nbTurns And mClefs(turn - 1, 0) <> dimCode 



                'on demande la nouvelle combi
                System.Console.WriteLine(" Saisissez une combinaison pour votre essai " & coup + 1 & " :")
                If coup = 0 Then
                    System.Console.WriteLine(" Exemple : vjrb")
                    System.Console.WriteLine(" Abandon : x")
                End If
                input = System.Console.ReadLine()
                If input = "x" Then 'si l'utilisateur souhaite abandonner
                    System.Console.WriteLine(" Abandon")
                    GoTo lblFin
                End If

                'on vérifie input grace a VerifInput
                Do While CheckInput(input, dimCode, mCouleurs, nbCol) = False
                    System.Console.Clear()
                    Display(nbCoups, dimCode, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, nbCol)
                    If input <> "x" Then : System.Console.WriteLine(" Votre proposition a été rejetée.") : End If
                    System.Console.WriteLine(" Saisissez une combinaison pour votre essai " & coup + 1 & " :")
                    input = System.Console.ReadLine()
                    If input = "x" Then
                        System.Console.WriteLine(" Abandon")
                        GoTo lblFin
                    End If
                Loop

                'on traite input et l'implémante dans jeu(,)
                For i = 0 To dimCode - 1
                    mCombi(i) = Conversion(Mid(input, i + 1, 1), mCouleurs, nbCol) 'on extrait une lettre dans input grace a Mid (ex : J dans NJRV)
                    mJeu(coup, i) = mCombi(i)                                      'et on la convertit grace à Conversion en entier dans combi() et jeu()
                Next

                'Calcul de noir
                mClefs(coup, 0) = Noire(mCode, mCombi, dimCode)

                'Calcul de blanc
                mClefs(coup, 1) = Blanche(mCode, mCombi, dimCode)

                'on passe au coup suivant
                coup = coup + 1

                'on affiche le plateau avec les nouvelles informations mJeu, mClefs, coup
                System.Console.Clear()
                Display(nbCoups, dimCode, DIMCODEMIN, mJeu, mClefs, coup, mCouleurs, nbCol)

                'on joue tant qu'on a pas atteint nbCoups coups ou que la clef noire vaut dimCode
            Loop While coup <> nbCoups And mClefs(coup - 1, 0) <> dimCode

lblFin:     If coup <> 0 Then 'Si aucun coup est joué on retourne directement au menu
                'On arrete le chrono
                chrono = 3600 * Date.Now.Hour + 60 * Date.Now.Minute + Date.Now.Second - chrono
                temp(0) = CStr(Floor(chrono / 60))
                temp(1) = CStr(chrono - 60 * Floor(chrono / 60))
                If CInt(temp(0)) = 0 Then
                    temp(2) = temp(1) & "s"
                Else
                    temp(2) = temp(0) & "min " & temp(1) & "s"
                End If

                'on transforme input en le code "brut" pour donner la réponse à l'utilisateur (recyclage de input)
                input = ""
                For i = 0 To dimCode - 1
                    input = input & mCouleurs(mCode(i), 0) 'ex : B>BN>BNJ>BNJV
                Next

                'message de fin de partie
                If mClefs(coup - 1, 0) = dimCode Then 'si la clé noire vaut la taille du code
                    System.Console.WriteLine(" Félicitation, la bonne combinaison est bien " & input & " ! :)")
                    victoire = True
                Else
                    If coup <> 1 Then : System.Console.WriteLine(" Dommage, tu n'as pas trouvé la bonne réponse en " & coup & " coups.") : Else : System.Console.WriteLine(" Dommage, tu n'as pas trouvé la bonne réponse en " & coup & " coup.") : End If
                    System.Console.WriteLine(" La bonne réponse était : " & input)
                    victoire = False
                End If

                System.Console.WriteLine(vbCrLf & " La partie a duré " & temp(2))
                'on enregistre la partie
                newSavedGame(GAMESPATH, dimCode, nbCoups, nbCol, mJeu, mCode)
                nbEnregistrees = nbEnregistrees + 1
                sessionCount = sessionCount + 1

                'Si les parametres sont ceux d'une partie classique
                If dimCode = DIMCODEMIN And nbCoups = COUPSMIN And nbCol = NBCOLMIN And doublons = False Then
                    updateStats(STATSPATH, liveStats, victoire, coup, chrono) 'on met à jour le fichier stats
                    liveStats.iniStats(STATSPATH)                             'et on met à jour les statistiques
                End If

                System.Console.ReadLine()

            End If

        Loop 'On retourne au menu

    End Sub


End Module

