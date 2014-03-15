
'MASTERMIND
'-----------------------------------
'ENSAM 2013
'Alexandre Callemeyn, David Hérault
'-----------------------------------
'Open source : GNU General Public License (GNU GPL)

Option Explicit On 'Declaration des variables obligatoire
Imports System     'Ajout des librairies au projet
Imports System.IO
Imports System.Text
Imports System.Math 'Celle là n'est pas par default (usitée pour sa fonction Round)

Module Fonctions

    'Initialise les couleurs, leurs initiales et entiers de référence
    Sub IniCol(ByRef couleurs(,) As String)

        couleurs(0, 0) = " "
        couleurs(1, 0) = "V"
        couleurs(2, 0) = "J"
        couleurs(3, 0) = "R"
        couleurs(4, 0) = "B"
        couleurs(5, 0) = "W"
        couleurs(6, 0) = "N"
        couleurs(7, 0) = "O"
        couleurs(8, 0) = "G"
        couleurs(9, 0) = "M"
        couleurs(10, 0) = "Z"

        couleurs(0, 1) = " "
        couleurs(1, 1) = "Vert"
        couleurs(2, 1) = "Jaune"
        couleurs(3, 1) = "Rouge"
        couleurs(4, 1) = "Bleu"
        couleurs(5, 1) = "Blanc"
        couleurs(6, 1) = "Noir"
        couleurs(7, 1) = "Orange"
        couleurs(8, 1) = "Gris"
        couleurs(9, 1) = "Marron"
        couleurs(10, 1) = "Rose"


    End Sub

    'Génère le code
    Sub IniCode(ByRef code() As Integer, ByVal dimcode As Integer, ByVal nbcol As Integer, ByVal doublons As Boolean)

        Dim i, j, x As Integer
        Dim check As Boolean = False
        Randomize()

        If doublons = True Then      'si on autorise les doublons
            For i = 0 To dimcode - 1
                code(i) = CInt(Int(nbcol * Rnd()) + 1) 'le code est simple a generer
            Next
        Else
            For i = 0 To dimcode - 1      'sinon pour chaque ligne de code()
                Do
                    x = CInt(Int(nbcol * Rnd()) + 1) 'on génère un entier caracteristique d'une couleur
                    check = True                     'ini check
                    For j = 0 To i - 1               'on parcourt code() du début à la ligne à completer
                        If x = code(j) Then          'si x existe déjà dans code()
                            check = False            'alors on boucle le Do pour regénèrer x
                        End If
                    Next j
                Loop Until check = True
                code(i) = x
            Next i
        End If


    End Sub

    'Calcul de la clef noire
    Function Noire(ByVal code() As Integer, ByVal combi() As Integer, ByVal dimm As Integer) As Integer

        Dim n As Integer = 0 'le nombre de couleurs bien placées
        Dim i As Integer

        For i = 0 To dimm - 1
            If combi(i) = code(i) Then 'si identité entre le code et la combinaison
                n = n + 1              'alors une couleur de plus est bien placée
            End If
        Next

        Return n

    End Function

    'Calcul de la clef blanche
    Function Blanche2(ByVal code() As Integer, ByVal combi() As Integer, ByVal dimm As Integer) As Integer

        'OYE OYE BRAVES GENS !!!
        'ON NE PEUX APPELLER UN TABLEAU EN BYVAL, IL EST DE TOUTES FACONS EN BYREF !
        'pk ? je ne sais pas.

        'Dim code(dimm - 1), combi(dimm - 1) As Integer
        Dim h As Integer = 0 'une variable de l'algo
        Dim n As Integer = 0 'la clef noire 
        Dim i, j As Integer
        Dim ausuivant As Boolean = False

        'For i = 0 To dimm - 1 'A SUPPRIMER !!!! mon dieu quel beug de m****
        '    code(i) = codee(i)
        '    combi(i) = combii(i)
        'Next

        'on calcul la clé noire
        For i = 0 To dimm - 1
            If code(i) = combi(i) Then
                n = n + 1
                code(i) = 0  'sauf qu'en cas d'itération de n, on évince les positions de code et combi correspondante
                combi(i) = 0 'pour ne pas les prendre en compte par la suite
            End If           'ce qui justifie le fait qu'on n'appelle pas Noire()
        Next

        For i = 0 To dimm - 1 'on parcourt combi() avec i

            If combi(i) = 0 Then 'si combi(i) a été annulé par le calcul de n on passe à combi(i+1)
                Continue For
            End If

            j = 0         'on initialise le parcourt de code() pour chaque combi(i)
            ausuivant = False

            While ausuivant = False
                If code(j) = combi(i) Then 'si il y a égalité entre code et combi
                    code(j) = 0            'on les évince
                    combi(i) = 0
                    '*
                    ausuivant = True       'et on passe à combi(i+1) directement
                End If
                j = j + 1
                If j = dimm Then           'si on a parcourut code(j) sans trouver d'egalité
                    ausuivant = True       'on passe à combi(i+1)
                End If
            End While
        Next i
        'Cette algorythme fonctionne mais n'est pas le plus rapide
        'on pourrait aussi calculer la clef blanche en ajoutant b = b + 1 au niveau de '*, on aurrait alors plus besoin de la ligne précédante
        'l'utilisation de While + ausuivant pourrait être remplacée par une boucle For j + Continue For
        'mais lors du debeug le Continue For affectait le For i de la boucle suppérieure (me semblait-il) <- pb reglé

        'J'ai refais une fonction clef blanche plus simple
        'mais j'ai encore un probleme
        'ma fonction clef blanche ecrit dans code et combi de Main() !!!
        'pourtant c'est du ByVal pas du ByRef
        'je me retrouve avec un code dans Main() valant 0000 après quelques 'Blanche()' ou 'Blanche2()'
        'c'est pour ca que je 'verse' codee et combii dans des variables de transfert code et combi

        For i = 0 To dimm - 1 'on compte avec h le nombre de case de combi restées non-nulles 
            If combi(i) <> 0 Then
                h = h + 1
            End If
        Next

        Return dimm - n - h 'algo maison, codé sur papier

    End Function

    'Nouveau calcul de la clef blanche
    Function Blanche(ByVal codee() As Integer, ByVal combii() As Integer, ByVal dimm As Integer) As Integer

        'OYE OYE BRAVES GENS !!!
        'ON NE PEUX APPELLER UN TABLEAU EN BYVAL, IL EST DE TOUTES FACONS EN BYREF !
        'pk ? je ne sais pas.

        Dim code(dimm), combi(dimm) As Integer 'ByVaaaaaaaaaaaaal !!!!
        Dim b As Integer = 0 'la valeur de la clé blanche
        Dim i, j As Integer

        For i = 0 To dimm - 1 'Le byref deviens byval
            code(i) = codee(i)
            combi(i) = combii(i)
        Next

        'on 'calcul' la clé noire
        For i = 0 To dimm - 1
            If code(i) = combi(i) Then
                code(i) = 0  'sauf qu'en cas d'itération de n, on évince les positions de code et combi correspondante
                combi(i) = 0 'pour ne pas les prendre en compte par la suite
            End If
        Next

        For i = 0 To dimm - 1 'on parcourt combi() avec i

            If combi(i) = 0 Then 'si combi(i) a été annulé par le calcul de n on passe à combi(i+1)
                Continue For
            End If

            For j = 0 To dimm - 1
                If code(j) = combi(i) Then 'si il y a égalité entre code et combi
                    code(j) = 0            'on évince code(j) pour ne pas eventuelement le reprendre en compte aux i suivants
                    b = b + 1              'on incrémente la clef blanche
                    Continue For           'et on passe à combi(i+1) directement
                End If
            Next j

        Next i

        Return b

    End Function

    'Convertit une lettre en l'entier associé grace à la matrice couleur
    Function Conversion(ByVal input As String, ByVal couleurs(,) As String, ByVal nbcol As Integer) As Integer

        Dim i As Integer

        For i = 1 To nbcol 'on parcours couleurs(i,0)
            If couleurs(i, 0) = input Then 'si une initiale correspond a l'input
                Return i                   'alors on renvoit son entier correspondant (position dans couleurs)
            End If
        Next

        Return 0

    End Function

    'Vérifie que les propositions de l'utilisateur ne créeront pas de pb
    Function CheckInput(ByRef input As String, ByVal dimcode As Integer, ByVal couleurs(,) As String, ByVal nbcol As Integer) As Boolean

        Dim i, j As Integer
        Dim x As Boolean = False

        input = UCase(Trim(input)) 'envèle les espaces externes et met en majuscule: "   njvr " -> "NJVR" 

        If Len(input) = dimcode Then 'si input fait la bonne taille

            For i = 0 To dimcode - 1        'pour chaque lettre de input()

                For j = 1 To nbcol                                'on prend chaque ligne de couleurs(,)
                    If Mid(input, i + 1, 1) = couleurs(j, 0) Then 'on verifie que la lettre de input corresponde à une couleur
                        x = True                                  'si c'est le cas x devient vrai
                        Exit For                                  'et on vérifie pas les couleurs restantes
                    End If
                Next j

                If x = False Then           'si il n'y a pas de correspondance x reste faux
                    Return False            'alors on renvoie False
                End If

                x = False 'ini prochaine lettre

            Next i

            Return True   'si pas de pb on renvoie true
        Else
            Return False  'si input n'a pas la bonne taille cest mort
        End If


    End Function

    'Vérifie que les parametres de l'utilisateur ne créeront pas de pb
    Function CheckInterval(ByRef input As String, ByVal min As Integer, ByVal max As Integer)

        input = Trim(input) 'envèle les espaces externes

        If IsNumeric(input) = True Then 'si c'est un nombre

            If CInt(input) >= min And CInt(input) <= max Then
                Return True
            Else
                Return False
            End If

        Else
            Return False
        End If

    End Function

    'Une procédure de temporisation
    Sub Wait(ByVal attente As Integer) 'attente en millisecondes
        Dim timer As Integer = 1000 * Date.Now.Second + Date.Now.Millisecond
        Do
            'rien ! A chercher : une méthode Wait() ou Sleep() 
        Loop While 1000 * Date.Now.Second + Date.Now.Millisecond - timer < attente
    End Sub

    'Affiche l'avancement du jeu
    Sub Display(ByVal COUPS As Integer, ByVal dimcode As Integer, ByVal DIMCODEMIN As Integer, ByVal jeu(,) As Integer, ByVal cle(,) As Integer, ByVal coup As Integer, ByVal couleurs(,) As String, ByVal nbcol As Integer)

        Dim i, j As Integer         'ajustement graphique :
        Dim x As String             'ex : " 9  " ou "10  "
        Dim jstring As String = "│" 'ex : |B|N|J|V|    j comme jeu
        Dim cstring As String       'ex : |0|2|        c comme clef
        Dim info As String          'ex : V : Vert
        Dim justi1 As String = "    " 'espacement entre | | | | | et | | | (constante)
        Dim justi2 As String = "   "  'espacement entre Essais et la derniere | de jstring
        Dim justi3 As String = "─"    'cadre supérieur de jeu(,)
        Dim justi4 As String = "─"    'cadre inférieur
        Dim justiCentrage As String   'espacement entre le bord gauche de la fenetre et le plateau

        'ini de justiCentrage
        justiCentrage = "          "
        For i = dimcode To 6
            justiCentrage = justiCentrage + " "
        Next

        'ini justi 2
        For i = 1 To dimcode - DIMCODEMIN
            justi2 = justi2 & "  " '2 espaces par difference entre dimcode et 4
        Next

        'ini du cadre de jeu(,)
        For i = 1 To dimcode - 1
            justi3 = justi3 & "┬─"
            justi4 = justi4 & "┴─"
        Next

        System.Console.WriteLine("░░░░░░░░░░░░░░░░░░░░░░░░MASTERMIND░░░░░░░░░░░░░░░░░░░░░░░░░" & vbCrLf & vbCrLf & vbCrLf)
        System.Console.WriteLine(justiCentrage & "    Essais" & justi2 & justi1 & " ☺ ☻  ")
        System.Console.WriteLine(justiCentrage & "    ┌" & justi3 & "┐" & justi1 & "┌─┬─┐")

        For i = 1 To COUPS 'on affiche les COUPS lignes
            '                x    jstring     cstring      info
            '               10  |N|B|V|R|       |3|0|  Z : Rose

            'ini de x 
            If i < 10 Then
                x = " " & CStr(i) & "  "  'A FAIRE : suppr les CStr <- Pourquoi ?
            Else
                x = CStr(i) & "  "
            End If

            'ini de info
            If i <= nbcol Then
                info = couleurs(i, 0) & " : " & couleurs(i, 1)
            Else
                info = ""
            End If

            'ini de jstring
            For j = 0 To dimcode - 1
                jstring = jstring & couleurs(jeu(i - 1, j), 0) & "│" ' | -> |N| -> |N|J| -> |N|J|J| -> ...
            Next j

            'ini de cstring
            If i <= coup Then 'si la ligne affichée correspond a un coup déjà joué on affiche les clefs
                cstring = "│" & CStr(cle(i - 1, 0)) & "│" & CStr(cle(i - 1, 1)) & "│  "
            Else
                cstring = "│ │ │  " 'sinon on laisse les deux cases vides
            End If

            System.Console.WriteLine(justiCentrage & x & jstring & justi1 & cstring & info) 'on affiche le tout

            'initialisation pour le next
            jstring = "│"
            cstring = ""
        Next i

        System.Console.WriteLine(justiCentrage & "    └" & justi4 & "┘" & justi1 & "└─┴─┘" & vbCrLf)

    End Sub

    'La visionneuse de parties
    Sub visioGame(ByVal file As String, ByVal DIMCODEMIN As Integer, ByVal NBCOLMAX As Integer, ByVal NBCOUPSMAX As Integer)

        'les variables de lecture
        Dim path As String = My.Application.Info.DirectoryPath & file 'le chemin des parties enregistrées
        Dim gameCount As Integer = countChar("#", file)               'le nombre de parties à visionner
        Dim game As Integer = gameCount                               'la partie lue
        Dim commaLine As String()                                     'la ligne lue
        Dim getKey As ConsoleKeyInfo                                  'une classe systeme pour lire les touches
        Dim temp As String                                            'temporaire
        Dim justi1 As String                                          'justification
        Dim justi2 As String = " "
        Dim i, j, c As Integer                                        'itérations

        'les variables dont les valeurs sont en mémoire morte
        Dim timedate As String
        Dim dimCode As Integer
        Dim nbCoups As Integer
        Dim nbCol As Integer
        Dim lastTurn As Integer

        'initialisation
        Console.Title = "Parties enregistrées"
        For i = 1 To 2 * Len(CStr(gameCount)) - 2
            justi2 = justi2 + " "
        Next

        'Le matrices pour recreer une partie
        'pour l'instant on ne connait pas leurs dimmensions
        Dim mCouleurs(NBCOLMAX + 1, 2) As String : IniCol(mCouleurs)
lbl:    Dim mJeu(1, 1) As Integer
        Dim mClefs(1, 1) As Integer
        Dim mCombi(1) As Integer
        Dim mCode(1) As Integer


        System.Console.Clear()

        'on ouvre le fichier
        Using Lecteur As New Microsoft.VisualBasic.FileIO.TextFieldParser(path)

            'on trouve la partie desirée
            c = 0
            Do
                If Lecteur.ReadLine = "#" Then
                    c = c + 1 'en incrementant c à chaque # rencontré (marqueur d'une partie)
                End If
            Loop Until c = game 'et en s'arretant quand on est à la partie voulue

            'le curseur se trouve alors sur la date de la partie
            'chaque ReadLine effectuant un carriage return on lit ainsi les infos stoquées
            timedate = Lecteur.ReadLine
            dimCode = CInt(Lecteur.ReadLine) 'plante le programme si l'utilisateur modifie le fichier
            nbCoups = CInt(Lecteur.ReadLine)
            nbCol = CInt(Lecteur.ReadLine)

            'ces infos permettent de redimensionner les matrices
            ReDim mJeu(nbCoups - 1, dimCode - 1)
            ReDim mClefs(nbCoups - 1, 1)
            ReDim mCombi(dimCode - 1)
            ReDim mCode(dimCode - 1)
            lastTurn = nbCoups 'par default le dernier coup est le 12eme en cas de defaite

            'la ligne suivante est le code
            Lecteur.TextFieldType = FileIO.FieldType.Delimited 'on lui indique que le texte à lire est délimité par un caractère
            Lecteur.SetDelimiters(",")
            commaLine = Lecteur.ReadFields() 'on lit la ligne du code, sectionnée par des comma
            For i = 0 To dimCode - 1         'et on la stoque dans mCode
                mCode(i) = CInt(commaLine(i))
            Next

            'lecture de la matrice jeu stoquée
            For i = 0 To nbCoups - 1                'pour chaque ligne
                commaLine = Lecteur.ReadFields()    'on lit la ligne segmentée

                For j = 0 To dimCode - 1            'pour chaque segment de la ligne (colonne)
                    mJeu(i, j) = CInt(commaLine(j)) 'on reconstruit mJeu
                    mCombi(j) = mJeu(i, j)          'et mCombi pour le calcul des clefs
                Next j
                'calcul des clefs
                mClefs(i, 0) = Noire(mCode, mCombi, dimCode)
                mClefs(i, 1) = Blanche(mCode, mCombi, dimCode)

                'si il y a victoire alors on met à jour dernierCoup
                If mClefs(i, 0) = dimCode Then
                    lastTurn = i + 1
                End If
                If mJeu(i, 0) = 0 Then
                    lastTurn = i
                    i = nbCoups
                Else
                    mClefs(i, 0) = Noire(mCode, mCombi, dimCode)
                    mClefs(i, 1) = Blanche(mCode, mCombi, dimCode)
                End If
            Next i

        End Using 'on ferme le fichier

        'on affiche le plateau
        Display(nbCoups, dimCode, DIMCODEMIN, mJeu, mClefs, lastTurn, mCouleurs, nbCol)

        'justification
        temp = Nothing
        For i = 1 To NBCOUPSMAX - nbCoups - 1
            temp = temp & vbCrLf
        Next
        If Not IsNothing(temp) Then 'pk le If ?
            System.Console.WriteLine(temp)
        End If

        'et les infos
        temp = Nothing
        justi1 = Nothing
        For i = 0 To dimCode - 1 'ecriture du code en clair
            temp = temp & mCouleurs(mCode(i), 0) & " "
        Next

        If gameCount > 9 Then 'justification
            For i = Len(CStr(game)) + 1 To Len(CStr(gameCount))
                justi1 = justi1 & " "
            Next
        End If

        System.Console.WriteLine(" Le code était :" & justi2 & temp)
        'justification
        temp = "──"
        For i = 1 To Len(CStr("◄ " & justi1 & game & "/" & gameCount & " ►"))
            temp = temp & "─"
        Next
        'affichage du cartouche
        System.Console.WriteLine("   ┌" & temp & "┐   Le " & timedate)
        System.Console.WriteLine("   │ ◄ " & justi1 & game & "/" & gameCount & " ► │   Quitter : [échap]")
        System.Console.WriteLine("   └" & temp & "┘")

        Do
            'inscrit la prochaine touche enfoncée dans getkey
            getKey = Console.ReadKey() 'Lorsqu'une touche est enfoncée la suite du code s'execute
            'on change de partie
            If getKey.Key = ConsoleKey.LeftArrow Then
                game = game - 1
                If game < 1 Then
                    game = gameCount
                End If
                GoTo lbl
            ElseIf getKey.Key = ConsoleKey.RightArrow Then
                game = game + 1
                If game > gameCount Then
                    game = 1
                End If
                GoTo lbl
            End If


        Loop While getKey.Key <> ConsoleKey.Escape

    End Sub

    'Affiche les regles
    Sub setRegles()

        Dim justi As String = " "
        Dim regles(36) As String
        Dim i As Integer

        regles(0) = "  LE MASTERMIND"
        regles(2) = "Le Mastermind est un jeu de logique et de déduction qui "
        regles(3) = "repose sur un principe très simple :"
        regles(4) = "retrouver une combinaison de 4 pions (parmi 6 couleurs"
        regles(5) = "possibles) en moins de 12 essais."
        regles(7) = "A chaque essai, le joueur reçoit des indications sur"
        regles(8) = "les couleurs et les emplacements qu’il a choisis :"
        regles(10) = "- un pion noir  (☺) indique un pion bien placé "
        regles(11) = "- un pion blanc (☻) indique un pion de la bonne couleur "
        regles(12) = "                    mais mal placé."
        regles(14) = "La stratégie consiste à choisir les couleurs et leur"
        regles(15) = "emplacement en fonction des coups précédents."
        regles(17) = "Le but est d’obtenir le plus d’informations et de se"
        regles(18) = "rapprocher le plus rapidement possible de la solution"
        regles(19) = "puisque le nombre de propositions est limité."
        regles(22) = "  LE SAVIEZ-VOUS ?"
        regles(24) = "Le Mastermind a été inventé par l'Israélien Mordecai "
        regles(25) = "Meirowitz dans les années 1960. Ce jeu de société a fait"
        regles(26) = "le tour du monde et a inspiré de nombreux autres jeux."
        regles(36) = "                                [Entrée] : retour au menu"

        Console.Title = "Règles du jeu"
        System.Console.Clear()
        System.Console.WriteLine()
        For i = 0 To regles.Length - 1
            System.Console.WriteLine(justi & regles(i))
        Next

        System.Console.ReadLine()
        System.Console.Clear()

    End Sub

    'affiche 'Bon Jeu!'
    Sub setBonJeu()
        System.Console.CursorVisible = False
        System.Console.WriteLine(vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf & vbCrLf)
        System.Console.WriteLine("        ____                     _              _ ")
        System.Console.WriteLine("       |  _ \                   | |            | |")
        System.Console.WriteLine("       | |_) | ___  _ __        | | ___ _   _  | |")
        System.Console.WriteLine("       |  _ < / _ \| '_ \   _   | |/ _ \ | | | | |")
        System.Console.WriteLine("       | |_) | (_) | | | | | |__| |  __/ |_| | |_|")
        System.Console.WriteLine("       |____/ \___/|_| |_|  \____/ \___|\__,_| (_)")
        Wait(800) 'On patiente            
        System.Console.Clear()
        System.Console.CursorVisible = True
    End Sub

End Module
