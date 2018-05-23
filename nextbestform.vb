Public Class nextbestform
    Private Sub Button1_Click(sender As Object, e As EventArgs)

        Dim rota(), visited(), y, nowat, totdist, mindist, nextat, sitep As Integer
        ReDim rota(n + 1)
        ReDim visited(n)
        If CheckBox1.Checked = True Then

            For y = 1 To n
                rota(1) = y
                rota(n + 1) = 1
                visited(y) = True
                For i = y + 1 To n
                    visited(i) = False
                Next i
                For i = y - 1 To 1 Step -1
                    visited(i) = False
                Next
                nowat = y
                totdist = 0
                For sitep = 2 To n
                    mindist = 1000000
                    For i = 1 To n
                        If i <> nowat And visited(i) = False Then
                            If tb(i, nowat).Text < mindist Then
                                nextat = i
                                mindist = tb(nextat, nowat).Text
                            End If
                        End If
                    Next i
                    rota(sitep) = nextat
                    visited(nextat) = True
                    totdist = totdist + mindist
                    nowat = nextat
                Next sitep
                For sitep = 1 To n

                    tb(sitep, n + y + 2) = New TextBox
                    tb(sitep, n + y + 2).Text = rota(sitep)
                    tb(sitep, n + y + 2).Location = New Point((40 * (sitep)) + 1, 21 * (n + y + 2))
                    tb(sitep, n + y + 2).Size = New Size(40, 20)
                    Me.Controls.Add(tb(sitep, n + y + 2))




                Next sitep
                tb(4 + sitep, n + y + 2) = New TextBox
                tb(4 + sitep, n + y + 2).Text = totdist + Int(p2((rota(n) - 1), 1).Text)
                tb(4 + sitep, n + y + 2).Location = New Point((40 * (sitep + 4)) + 1, 21 * (n + y + 2))
                tb(4 + sitep, n + y + 2).Size = New Size(40, 20)
                Me.Controls.Add(tb(4 + sitep, n + y + 2))



            Next y
        End If

        If CheckBox1.Checked = False Then


            For y = 1 To n
                rota(1) = y
                rota(n + 1) = 1
                visited(y) = True
                For i = y + 1 To n
                    visited(i) = False
                Next i
                For i = y - 1 To 1 Step -1
                    visited(i) = False
                Next
                nowat = y
                totdist = 0
                For sitep = 2 To n
                    mindist = 1000000
                    For i = 1 To n
                        If i <> nowat And visited(i) = False Then
                            If tb(i, nowat).Text < mindist Then
                                nextat = i
                                mindist = tb(nextat, nowat).Text
                            End If
                        End If
                    Next i
                    rota(sitep) = nextat
                    visited(nextat) = True
                    totdist = totdist + mindist
                    nowat = nextat
                Next sitep
                For sitep = 1 To n

                    tb(sitep, n + y + 2) = New TextBox
                    tb(sitep, n + y + 2).Text = rota(sitep)
                    tb(sitep, n + y + 2).Location = New Point((40 * (sitep)) + 1, 21 * (n + y + 2))
                    tb(sitep, n + y + 2).Size = New Size(40, 20)
                    Me.Controls.Add(tb(sitep, n + y + 2))




                Next sitep
                tb(4 + sitep, n + y + 2) = New TextBox
                tb(4 + sitep, n + y + 2).Text = totdist
                tb(4 + sitep, n + y + 2).Location = New Point((40 * (sitep + 4)) + 1, 21 * (n + y + 2))
                tb(4 + sitep, n + y + 2).Size = New Size(40, 20)
                Me.Controls.Add(tb(4 + sitep, n + y + 2))



            Next y
        End If







    End Sub





    Private Sub SimpleButton2_Click(sender As Object, e As EventArgs)

        n = ComboBox1.SelectedItem
        For i = 1 To n
            For j = 1 To n
                tb(i, j) = New TextBox
                tb(i, j).Name = String.Format("box", i.ToString, j.ToString)
                tb(i, j).Text = ""
                tb(i, j).Size = New Size(40, 20)
                tb(i, j).Location = New Point((40 * i) + 1, 21 * j)
                tb(i, j).BorderStyle = BorderStyle.Fixed3D
                Me.Controls.Add(tb(i, j))
            Next j
        Next i
        For i = 0 To n - 1
            lb(i, 1) = New Label
            lb(i, 1).Text = "P(" & i + 1 & ")"
            lb(i, 1).Size = New Size(40, 20)
            lb(i, 1).Location = New Point((0) + 1, 21 * (i + 1))
            lb(i, 1).BackColor = Color.Aqua
            Me.Controls.Add(lb(i, 1))

        Next i
        For j = 0 To n - 1
            lb(23, j) = New Label
            lb(23, j).Text = "P(" & j + 1 & ")"
            lb(23, j).Size = New Size(40, 20)
            lb(23, j).Location = New Point((40 * (j + 1)) + 1, 0 + 1)

            lb(23, j).BackColor = Color.Aqua
            Me.Controls.Add(lb(23, j))

        Next j
        For i = 1 To n
            tb(i, i).Text = 0
        Next

    End Sub



    Private Sub Button2_Click(sender As Object, e As EventArgs)
        n = ComboBox1.SelectedItem
        For i = 0 To n - 1
            p2(i, 1) = New TextBox
            p2(i, 1).Text = Int(Rnd() * 50)
            p2(i, 1).Size = New Size(40, 20)
            p2(i, 1).Location = New Point(560, 21 * (i + 3))
            p2(i, 1).BackColor = Color.Khaki
            Me.Controls.Add(p2(i, 1))
        Next i
    End Sub

    Private Sub SimpleButton1_Click(sender As Object, e As EventArgs)
        If CheckBox1.Checked = True Then
            n = ComboBox1.SelectedItem

            For j = 1 To n
                For i = 1 To n

                    tb(i, j).Text = Int(tb(i, j).Text) + Int(p2(j - 1, 1).Text)


                Next i
            Next j
            For i = 1 To n
                tb(i, i).Text = 0
            Next
        End If
        If CheckBox1.Checked = False Then MsgBox("İşlem Süresi Ekleme Aktif Değil!", MsgBoxStyle.MsgBoxHelp)
    End Sub

    Private Sub PictureBox1_Click(sender As Object, e As EventArgs)
        Me.Close()
    End Sub

    Private Sub SimpleButton6_Click(sender As Object, e As EventArgs) Handles SimpleButton6.Click
        Me.Close()
        Main.Show()
    End Sub

    Private Sub BunifuFlatButton1_Click(sender As Object, e As EventArgs) Handles BunifuFlatButton1.Click
        n = ComboBox1.SelectedItem
        For i = 1 To n
            For j = 1 To n
                tb(i, j) = New TextBox
                tb(i, j).Name = String.Format("box", i.ToString, j.ToString)
                tb(i, j).Text = ""
                tb(i, j).Size = New Size(40, 20)
                tb(i, j).Location = New Point((40 * i) + 1, 21 * j)
                tb(i, j).BorderStyle = BorderStyle.Fixed3D
                Me.Controls.Add(tb(i, j))
            Next j
        Next i
        For i = 0 To n - 1
            lb(i, 1) = New Label
            lb(i, 1).Text = "P(" & i + 1 & ")"
            lb(i, 1).Size = New Size(40, 20)
            lb(i, 1).Location = New Point((0) + 1, 21 * (i + 1))
            lb(i, 1).BackColor = Color.Aqua
            Me.Controls.Add(lb(i, 1))

        Next i
        For j = 0 To n - 1
            lb(23, j) = New Label
            lb(23, j).Text = "P(" & j + 1 & ")"
            lb(23, j).Size = New Size(40, 20)
            lb(23, j).Location = New Point((40 * (j + 1)) + 1, 0 + 1)

            lb(23, j).BackColor = Color.Aqua
            Me.Controls.Add(lb(23, j))

        Next j
        For i = 1 To n
            tb(i, i).Text = 0
        Next

    End Sub

    Private Sub SimpleButton3_Click(sender As Object, e As EventArgs)


        n = ComboBox1.SelectedItem
        For i = 1 To n
            For j = 1 To n

                tb(i, j).Text = Int(Rnd() * 50) + 1

            Next j
        Next i
        For i = 1 To n
            tb(i, i).Text = 0
        Next




        For i = 1 To n
            tb(i, i).Text = 0
        Next
    End Sub

    Private Sub BunifuFlatButton2_Click(sender As Object, e As EventArgs) Handles BunifuFlatButton2.Click
        n = ComboBox1.SelectedItem
        For i = 1 To n
            For j = 1 To n

                tb(i, j).Text = Int(Rnd() * 50) + 1

            Next j
        Next i
        For i = 1 To n
            tb(i, i).Text = 0
        Next




        For i = 1 To n
            tb(i, i).Text = 0
        Next
    End Sub

    Private Sub BunifuFlatButton3_Click(sender As Object, e As EventArgs) Handles BunifuFlatButton3.Click
        n = ComboBox1.SelectedItem
        For i = 0 To n - 1
            p2(i, 1) = New TextBox
            p2(i, 1).Text = Int(Rnd() * 50)
            p2(i, 1).Size = New Size(40, 20)
            p2(i, 1).Location = New Point(560 + (42 * (i + 3)), 50)
            p2(i, 1).BackColor = Color.Khaki
            Me.Controls.Add(p2(i, 1))
        Next i
        Label2.Visible = True
    End Sub

    Private Sub BunifuFlatButton5_Click(sender As Object, e As EventArgs) Handles BunifuFlatButton5.Click
        Dim rota(), visited(), y, nowat, totdist, mindist, nextat, sitep As Integer
        ReDim rota(n + 1)
        ReDim visited(n)
        If CheckBox1.Checked = True Then

            For y = 1 To n
                rota(1) = y
                rota(n + 1) = 1
                visited(y) = True
                For i = y + 1 To n
                    visited(i) = False
                Next i
                For i = y - 1 To 1 Step -1
                    visited(i) = False
                Next
                nowat = y
                totdist = 0
                For sitep = 2 To n
                    mindist = 1000000
                    For i = 1 To n
                        If i <> nowat And visited(i) = False Then
                            If tb(i, nowat).Text < mindist Then
                                nextat = i
                                mindist = tb(nextat, nowat).Text
                            End If
                        End If
                    Next i
                    rota(sitep) = nextat
                    visited(nextat) = True
                    totdist = totdist + mindist
                    nowat = nextat
                Next sitep
                For sitep = 1 To n

                    tb(sitep, n + y + 2) = New TextBox
                    tb(sitep, n + y + 2).Text = rota(sitep)
                    tb(sitep, n + y + 2).Location = New Point((40 * (sitep)) + 1, 21 * (n + y + 2))
                    tb(sitep, n + y + 2).Size = New Size(40, 20)
                    Me.Controls.Add(tb(sitep, n + y + 2))




                Next sitep
                tb(4 + sitep, n + y + 2) = New TextBox
                tb(4 + sitep, n + y + 2).Text = totdist + Int(p2((rota(n) - 1), 1).Text)
                tb(4 + sitep, n + y + 2).Location = New Point((40 * (sitep + 4)) + 1, 21 * (n + y + 2))
                tb(4 + sitep, n + y + 2).Size = New Size(40, 20)
                Me.Controls.Add(tb(4 + sitep, n + y + 2))



            Next y
        End If

        If CheckBox1.Checked = False Then


            For y = 1 To n
                rota(1) = y
                rota(n + 1) = 1
                visited(y) = True
                For i = y + 1 To n
                    visited(i) = False
                Next i
                For i = y - 1 To 1 Step -1
                    visited(i) = False
                Next
                nowat = y
                totdist = 0
                For sitep = 2 To n
                    mindist = 1000000
                    For i = 1 To n
                        If i <> nowat And visited(i) = False Then
                            If tb(i, nowat).Text < mindist Then
                                nextat = i
                                mindist = tb(nextat, nowat).Text
                            End If
                        End If
                    Next i
                    rota(sitep) = nextat
                    visited(nextat) = True
                    totdist = totdist + mindist
                    nowat = nextat
                Next sitep
                For sitep = 1 To n

                    tb(sitep, n + y + 2) = New TextBox
                    tb(sitep, n + y + 2).Text = rota(sitep)
                    tb(sitep, n + y + 2).Location = New Point((40 * (sitep)) + 1, 21 * (n + y + 2))
                    tb(sitep, n + y + 2).Size = New Size(40, 20)
                    Me.Controls.Add(tb(sitep, n + y + 2))




                Next sitep
                tb(4 + sitep, n + y + 2) = New TextBox
                tb(4 + sitep, n + y + 2).Text = totdist
                tb(4 + sitep, n + y + 2).Location = New Point((40 * (sitep + 4)) + 1, 21 * (n + y + 2))
                tb(4 + sitep, n + y + 2).Size = New Size(40, 20)
                Me.Controls.Add(tb(4 + sitep, n + y + 2))



            Next y
        End If



    End Sub

    Private Sub BunifuFlatButton4_Click(sender As Object, e As EventArgs) Handles BunifuFlatButton4.Click
        If CheckBox1.Checked = True Then
            n = ComboBox1.SelectedItem

            For j = 1 To n
                For i = 1 To n

                    tb(i, j).Text = Int(tb(i, j).Text) + Int(p2(j - 1, 1).Text)


                Next i
            Next j
            For i = 1 To n
                tb(i, i).Text = 0
            Next
        End If
        If CheckBox1.Checked = False Then MsgBox("İşlem Süresi Ekleme Aktif Değil!", MsgBoxStyle.MsgBoxHelp)
    End Sub
End Class