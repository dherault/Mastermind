
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

Module StatsEtSauvegarde
    'Une fonction générique qui compte le nombre d'occurence d'une ligne dans un fichier texte
    Function countChar(ByVal neededline As String, ByVal file As String) As Integer

        Dim path As String = My.Application.Info.DirectoryPath & file 'le chemin des parties enregistrées
        Dim line As String 'la ligne lue
        Dim c As Integer = 0

        Using Lecteur As New Microsoft.VisualBasic.FileIO.TextFieldParser(path) 'TextFielParser est une n-eme methode de lecture de fichier

            While Not Lecteur.EndOfData    'tant qu'on a pas lu tout le texte 
                line = Lecteur.ReadLine 'on lit la ligne actuel et on passe à la suivante
                If line = neededline Then 'si les lignes correspondent
                    c = c + 1 'c++
                End If
            End While

        End Using 'Drugs are bad mkay... On kill le process de lecture

        Return c

    End Function

    'définie le texte enregistré dans le fichier statistique
    Function getStatsSaveText(ByVal choice As Integer) As String

        Dim output As String
        If choice = 0 Then
            output = "Toute modification apportée à ce fichier peut entrainer une instabilité du programme : "
        ElseIf choice = 1 Then
            output = "Nombre de parties jouées : "
        ElseIf choice = 2 Then
            output = "Nombre de victoires : "
        ElseIf choice = 3 Then
            output = "Nombre de coups cumulés des victoires : "
        ElseIf choice = 4 Then
            output = "Meilleur score en coups : "
        ElseIf choice = 5 Then
            output = "Temps total cumulé de jeu : "
        Else
            output = Nothing
        End If

        Return output

    End Function

    'Initialise la sauvergarde
    Sub iniSave(ByVal statSaveName As String, ByVal gameSaveName As String)

        Dim statPath As String = My.Application.Info.DirectoryPath & statSaveName 'le chemin des statistiques
        Dim gamePath As String = My.Application.Info.DirectoryPath & gameSaveName 'le chemin des parties enregistrées
        Dim i As Integer

        If My.Computer.FileSystem.FileExists(statPath) = False Then 'si le fichier stats n'existe pas
            Dim txtStats As FileStream = File.Create(statPath) 'on le crée (donc on l'ouvre)
            txtStats.Close() 'puis on le ferme
            For i = 0 To 5
                My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(i) & "0" & vbCrLf, True) 'pour pouvoir l'ouvrir avec une autre methode dont la syntaxe est plus simple
            Next
        End If

        If My.Computer.FileSystem.FileExists(gamePath) = False Then 'si le fichier parties n'existe pas
            Dim txtGame As FileStream = File.Create(gamePath) 'on le crée (donc on l'ouvre)
            txtGame.Close() 'puis on le ferme
            My.Computer.FileSystem.WriteAllText(gamePath, getStatsSaveText(0) & vbCrLf, False)
        End If

    End Sub

    'Sauvegarde une partie
    Sub newSavedGame(ByVal saveName As String, ByVal dimCode As Integer, ByVal nbCoups As Integer, ByVal nbCol As Integer, ByVal mJeu(,) As Integer, ByVal mCode() As Integer)

        Dim i, j As Integer                                               'itération
        Dim newline As String                                             'une ligne
        Dim path As String = My.Application.Info.DirectoryPath & saveName 'le chemin du fichier PartiesEnregistrees.txt
        Dim code As String = CStr(mCode(0))                               'code = a

        'on ecrit le code sous la forme "a,b,c,d"
        For i = 1 To dimCode - 1
            code = code & "," & CStr(mCode(i))
        Next
        'puis les infos de la partie
        My.Computer.FileSystem.WriteAllText(path, vbCrLf & "#" & vbCrLf, True) 'un # marque le début de la partie
        My.Computer.FileSystem.WriteAllText(path, Now.Day & "/" & Now.Month & "/" & Now.Year & " à " & Now.Hour & "h" & Now.Minute & vbCrLf, True) 'la date et l'heure de la partie
        My.Computer.FileSystem.WriteAllText(path, CStr(dimCode) & vbCrLf, True) 'on inscrit les dimmensions de la matrice jeu
        My.Computer.FileSystem.WriteAllText(path, CStr(nbCoups) & vbCrLf, True) 'dimcode et nbcoups
        My.Computer.FileSystem.WriteAllText(path, CStr(nbCol) & vbCrLf, True) 'le nombre de couleurs
        My.Computer.FileSystem.WriteAllText(path, code & vbCrLf, True) 'on entre le code
        For i = 0 To nbCoups - 1 'et la matrice jeu
            newline = CStr(mJeu(i, 0)) 'newline = f
            For j = 1 To dimCode - 1 'pour chaque colonne
                newline = newline & "," & CStr(mJeu(i, j)) 'on ecrit chaque ligne de mJeu sous la forme f,b,c,d
            Next j
            My.Computer.FileSystem.WriteAllText(path, newline & vbCrLf, True)
        Next i

    End Sub

    'Met à jour le fichier stats après une partie
    Sub updateStats(ByVal savename As String, ByVal statistiques As Stats, ByVal victoire As Boolean, ByVal coup As Integer, ByVal time As Integer)

        Dim statPath As String = My.Application.Info.DirectoryPath & savename 'le chemin des statistiques
        'en gros le code ecrit "dutextedutextedutext : variable"
        My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(0) & "0" & vbCrLf, False)
        'nombre de parties jouees
        My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(1) & CStr(statistiques.nbPartiesJouees + 1) & vbCrLf, True)
        'nombre de victoire et de coup
        If victoire = True Then
            statistiques.nbVictoires = statistiques.nbVictoires + 1
            My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(2) & CStr(statistiques.nbVictoires) & vbCrLf, True)
            My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(3) & CStr(statistiques.nbCoupsVictoire + coup) & vbCrLf, True)
        Else
            My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(2) & CStr(statistiques.nbVictoires) & vbCrLf, True)
            My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(3) & CStr(statistiques.nbCoupsVictoire) & vbCrLf, True)
        End If
        'meilleur coup
        If victoire = True Then
            If coup < statistiques.meilleurScore Or statistiques.meilleurScore = 0 Then
                My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(4) & CStr(coup) & vbCrLf, True)
            Else
                My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(4) & CStr(statistiques.meilleurScore) & vbCrLf, True)
            End If
        Else
            My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(4) & CStr(statistiques.meilleurScore) & vbCrLf, True)
        End If
        'temp de jeu cumulé
        My.Computer.FileSystem.WriteAllText(statPath, getStatsSaveText(5) & CStr(statistiques.tempsTotal + time) & vbCrLf, True)

    End Sub

    'Lis les différentes données dans le fichier statistiques
    Function readStats(ByVal choice As Integer, ByVal STATSAVENAME As String) As Integer

        Dim path As String = My.Application.Info.DirectoryPath & STATSAVENAME 'le chemin des parties enregistrées
        Dim line As String() = Nothing 'la ligne lue tableau non definie
        Dim i As Integer

        Using Lecteur As New Microsoft.VisualBasic.FileIO.TextFieldParser(path) 'TextFielParser est une n-eme methode de lecture de fichier
            Lecteur.TextFieldType = FileIO.FieldType.Delimited 'on lui indique que le texte à lire est délimité par un caractère

            'ce caractère sera :
            Lecteur.SetDelimiters(" : ")

            For i = 0 To choice 'de la premiere ligne à celle qui nous interesse 
                line = Lecteur.ReadFields() 'on stock la ligne segmentée dans un array line() (ReadFiels() place le curseur sur la ligne suivante après la lecture)
            Next

        End Using 'On kill le process de lecture

        Return CInt(line(1)) 'la premiere valeur est "nb de victoires" (index 0), tandis que la seconde (index 1) nous interesse

    End Function

    'Defini la classe utilisée pour le calcul des statistiques
    Class Stats

        Public nbPartiesJouees As Integer  'le nombre de parties jouées
        Public nbVictoires As Integer      'le nombre de parties gagnées
        Public nbCoupsVictoire As Integer  'le total du nombre de coups des parties victorieuses
        Public meilleurScore As Integer    'le plus petit nombre de coups
        Public tempsTotal As Integer       'la durée de toutes les parties

        Public line1 As String             'les lignes sous 'STATISTIQUES' dans le menu
        Public line2 As String
        Public line3 As String
        Public line4 As String
        Public line5 As String

        'Initialise les linei en fonction de la memoire morte
        Public Sub iniStats(ByVal STATSAVENAME As String)

            Dim pourcentagevictoire As Integer 'le pourcentage de victoire
            Dim tempsmoyen As Single           'la durée moyenne d'une partie en seconde
            Dim minutes As Integer             'et sa conversion en minutes
            Dim secondes As Integer            'et secondes
            Dim txt1 As String                 'texte de justification du pluriel/singulier
            Dim txt2 As String

            'Lecture des valeurs en mémoire morte
            Me.nbPartiesJouees = readStats(1, STATSAVENAME)
            Me.nbVictoires = readStats(2, STATSAVENAME)
            Me.nbCoupsVictoire = readStats(3, STATSAVENAME)
            Me.meilleurScore = readStats(4, STATSAVENAME)
            Me.tempsTotal = readStats(5, STATSAVENAME)

            'Calcul des différentes statistiques
            If nbPartiesJouees = 0 Then
                pourcentagevictoire = 100
            Else
                pourcentagevictoire = CInt(Round(100 * nbVictoires / nbPartiesJouees))
            End If

            'justification 
            If Me.nbPartiesJouees <= 1 Then
                txt1 = " partie jouée dont "
            Else
                txt1 = " parties jouées dont "
            End If

            If Me.nbVictoires <= 1 Then
                txt2 = " de gagnée ("
            Else
                txt2 = " de gagnées ("
            End If

            'ligne 1
            Me.line1 = CStr(Me.nbPartiesJouees) & txt1 & CStr(Me.nbVictoires) & txt2 & CStr(pourcentagevictoire) & "%)"
            'ligne 2
            If Me.meilleurScore = 0 Then
                Me.line2 = "Pas de meilleur score"
            Else
                Me.line2 = "Meilleur score           : " & CStr(Me.meilleurScore)
            End If
            'ligne 3
            If Me.nbVictoires = 0 Then
                Me.line3 = "Pas de score moyen pour les victoires"
            Else
                Me.line3 = "Score moyen - victoires  : " & CStr(Round(Me.nbCoupsVictoire / Me.nbVictoires, 1))
            End If
            'ligne 4
            If Me.nbPartiesJouees = 0 Then
                Me.line4 = "Pas de score moyen total"
                tempsmoyen = 0
            Else 'les parties perdues se font en 12 coups (meme si c'est faux...)
                Me.line4 = "Score moyen - total      : " & CStr(Round((Me.nbCoupsVictoire + 12 * (Me.nbPartiesJouees - Me.nbVictoires)) / Me.nbPartiesJouees, 1)) & " coups"
                tempsmoyen = Me.tempsTotal / Me.nbPartiesJouees
            End If

            'conversion secondes -> minutes et secondes
            minutes = Floor(tempsmoyen / 60)
            secondes = Floor(tempsmoyen - 60 * minutes)
            'ligne 5
            If minutes = 0 And secondes = 0 Then
                Me.line5 = "Pas de temps moyen"
            Else
                Me.line5 = "Temps moyen d'une partie : " & CStr(minutes) & "min " & CStr(secondes) & "s"
            End If

        End Sub

    End Class
End Module
