Imports MySql.Data.MySqlClient
Public Class Form1
    Dim da As MySqlDataAdapter
    Dim Conn As MySqlConnection
    Dim ds As DataSet
    Dim CMD As MySqlCommand
    Dim RD As MySqlDataReader
    Dim LokasiDB As String
    Dim table As DataTable
    Sub Koneksi()
        LokasiDB = "Server=localhost;Port=;Database=atp;Uid=root;Pwd=;"
        Conn = New MySqlConnection(LokasiDB)
        If Conn.State = ConnectionState.Closed Then
            Conn.Open()
        End If

    End Sub
    Private Sub pctMOBIL2_Tick(sender As Object, e As EventArgs) Handles tmrmobilcepat.Tick

        Try
            Dim a As Integer
            a = 1

            pctSPORT.Left = pctSPORT.Left - 6
            If pctSPORT.Left = -106 Then
                pctSPORT.Left = 1316
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub tmrmobilbiasa_Tick(sender As Object, e As EventArgs) Handles tmrmobilbiasa.Tick
        Try
            pctMBIASA.Left = pctMBIASA.Left - 4
            If pctMBIASA.Left = -104 Then
                pctMBIASA.Left = 1316
                pctMBIASA.BackgroundImage = Image.FromFile("minicooper.png")
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub tmrmobillama_Tick(sender As Object, e As EventArgs) Handles tmrmobillama.Tick
        Try
            pctMLAMA.Left = pctMLAMA.Left - 2
            If pctMLAMA.Left = -106 Then
                pctMLAMA.Left = 1316
                pctMLAMA.BackgroundImage = Image.FromFile("car.png")
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub tmrpolisi_Tick(sender As Object, e As EventArgs) Handles tmrpolisi.Tick
        Try
            pctMOBILPOL.Left = pctMOBILPOL.Left - 1

            If pctMOBILPOL.Left = -106 Then
                pctMOBILPOL.Left = 1316
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub tmrMOBILAPT_Tick(sender As Object, e As EventArgs) Handles tmrMOBILAPT.Tick
        Try
            pctMOBILAPT.Left = pctMOBILAPT.Left + 2
            If txtSTATUS.Text = "close" Then
                If pctMOBILAPT.Left >= 236 And pctMOBILAPT.Left <= 238 Then
                    Try
                        Call Koneksi()
                        Dim update As String = "update account set lokasi='" & labelgate.Text & "' where nohp = '" & txtPENGEMUDI.Text & "'"
                        CMD = New MySqlCommand(update, Conn)
                        CMD.ExecuteNonQuery()
                    Catch ex As Exception

                    End Try
                ElseIf pctMOBILAPT.Left >= 238 And pctMOBILAPT.Left <= 248 Then
                    tmrMOBILAPT.Enabled = False
                    tmrMOBILAPT2.Enabled = True
                ElseIf pctMOBILAPT.Left = 1360 Then
                    tmrMOBILAPT.Enabled = False
                    TmrCHECKDB.Enabled = False
                End If
            End If
        Catch ex As Exception

        End Try


    End Sub

    Public Function ToMySql(d As Date) As String
        Return d.ToString("yyyy-MM-dd HH:mm:ss")
    End Function


    Private Sub tmrMOBILAPT2_Tick(sender As Object, e As EventArgs) Handles tmrMOBILAPT2.Tick
        Dim tanggal As String
        tanggal = Format(Now, "yyyy–mm–dd") & " " & Format(Now, "hh:mm:ss")
        Try
            pctMOBILAPT.Left = pctMOBILAPT.Left + 1
            If pctMOBILAPT.Left = 462 Then
                Try
                    Call Koneksi()
                    If (lbltopay.Text - lblharga.Text) >= 0 Then
                        
                        Dim updategate As String = "UPDATE gate SET status = 'open' where nama = '" & labelgate.Text & "'"
                        CMD = New MySqlCommand(updategate, Conn)
                        CMD.ExecuteNonQuery()
                        Dim inserttrans As String = "INSERT INTO transaksi (nohp,gatename,tanggal,harga) VALUES ('" & txtPENGEMUDI.Text & "','" & labelgate.Text & "','" & DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss") & "', " & lblharga.Text & ")"
                        CMD = New MySqlCommand(inserttrans, Conn)
                        CMD.ExecuteNonQuery()
                    ElseIf (lbltopay.Text - lblharga.Text) < 0 Then
                        If (lblpulsa.Text - lblharga.Text) >= 0 Then
                            Dim updategate As String = "UPDATE gate SET status = 'open' where nama = '" & labelgate.Text & "'"
                            CMD = New MySqlCommand(updategate, Conn)
                            CMD.ExecuteNonQuery()
                            Dim inserttrans As String = "INSERT INTO transaksi (nohp,gatename,tanggal,harga) VALUES ('" & txtPENGEMUDI.Text & "','" & labelgate.Text & "','" & DateTime.Now.ToString("yyyy-MM-dd HH:mm:ss") & "', " & lblharga.Text & ")"
                            CMD = New MySqlCommand(inserttrans, Conn)
                            CMD.ExecuteNonQuery()
                        Else

                        End If
                    End If


                Catch ex As Exception

                End Try
            ElseIf pctMOBILAPT.Left = 670 Then
                Try
                    Call Koneksi()
                    If (lbltopay.Text - lblharga.Text) >= 0 Then
                        Dim update As String = "UPDATE account SET saldo = saldo - " & lblharga.Text & " WHERE nohp = '" & txtPENGEMUDI.Text & "'"
                        CMD = New MySqlCommand(update, Conn)
                        CMD.ExecuteNonQuery()
                        saldoakun()
                    ElseIf (lbltopay.Text - lblharga.Text) < 0 Then
                        If (lblpulsa.Text - lblharga.Text) >= 0 Then
                            Dim update As String = "UPDATE provider SET saldoprov = saldoprov - " & lblharga.Text & " WHERE nohpprov = '" & txtPENGEMUDI.Text & "'"
                            CMD = New MySqlCommand(update, Conn)
                            CMD.ExecuteNonQuery()
                            saldoprovider()
                        End If
                    End If
                Catch ex As Exception

                End Try

            ElseIf pctMOBILAPT.Left = 680 And palang.Height > 20 Then
                tmrMOBILAPT2.Enabled = False
               
                

            ElseIf pctMOBILAPT.Left >= 846 And pctMOBILAPT.Left <= 850 Then
                tmrPALANG2.Enabled = True
            ElseIf pctMOBILAPT.Left = 860 Then
                tmrMOBILAPT2.Enabled = False
                tmrMOBILAPT.Enabled = True
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub tmrPALANG_Tick(sender As Object, e As EventArgs) Handles tmrPALANG.Tick
        Try
            palang.Height = palang.Height - 1
            If palang.Height = 20 Then
                Try
                    Call Koneksi()
                    Dim update As String = "update account set lokasi='none' where nohp = '" & txtPENGEMUDI.Text & "'"
                    CMD = New MySqlCommand(update, Conn)
                    CMD.ExecuteNonQuery()
                Catch ex As Exception

                End Try
                tmrPALANG.Enabled = False
                LAMPUAPT.BackColor = Color.GreenYellow
                tmrMOBILAPT2.Enabled = True
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Try
            tmrMOBILAPT.Enabled = False
            pctMOBILAPT.Top = 326
            pctMOBILAPT.Left = 12
            TmrCHECKDB.Enabled = False
            Try
                Call Koneksi()
                Dim update As String = "update account set lokasi='none' where nohp = '" & txtPENGEMUDI.Text & "'"
                CMD = New MySqlCommand(update, Conn)
                CMD.ExecuteNonQuery()
            Catch ex As Exception

            End Try
        Catch ex As Exception

        End Try
    End Sub

    Private Sub tmrPALANG2_Tick(sender As Object, e As EventArgs) Handles tmrPALANG2.Tick
        Try
            palang.Height = palang.Height + 1
            If palang.Height = 70 Then
                tmrPALANG2.Enabled = False
                statusgateclose()
                LAMPUAPT.BackColor = Color.Red
                TmrCHECKDB.Enabled = True
                
            End If
        Catch ex As Exception

        End Try

    End Sub

    Private Sub TmrCHECKDB_Tick(sender As Object, e As EventArgs) Handles TmrCHECKDB.Tick
        Try
            statusgate()
            If txtSTATUS.Text = "open" Then
                TmrCHECKDB.Enabled = False
                tmrPALANG.Enabled = True
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub statusgate()
        Try
            Call Koneksi()
            LokasiDB = "select status from gate where nama = '" & labelgate.Text & "'"
            CMD = New MySqlCommand(LokasiDB, Conn)
            RD = CMD.ExecuteReader
            RD.Read()
            If RD.HasRows Then
                txtSTATUS.Text = RD.Item("status")
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub saldoakun()
        Try
            Call Koneksi()
            LokasiDB = "select *from account where nohp = '" & txtPENGEMUDI.Text & "'"
            CMD = New MySqlCommand(LokasiDB, Conn)
            RD = CMD.ExecuteReader
            RD.Read()
            If RD.HasRows Then
                lbltopay.Text = RD.Item("saldo")
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub hargagate()
        Try
            Call Koneksi()
            LokasiDB = "select *from gate where nama = '" & labelgate.Text & "'"
            CMD = New MySqlCommand(LokasiDB, Conn)
            RD = CMD.ExecuteReader
            RD.Read()
            If RD.HasRows Then
                lblharga.Text = RD.Item("harga")
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub saldoprovider()
        Try
            Call Koneksi()
            LokasiDB = "select *from provider where nohpprov = '" & txtPENGEMUDI.Text & "'"
            CMD = New MySqlCommand(LokasiDB, Conn)
            RD = CMD.ExecuteReader
            RD.Read()
            If RD.HasRows Then
                lblpulsa.Text = RD.Item("saldoprov")
            End If
        Catch ex As Exception

        End Try

    End Sub
    Sub statusgateclose()
        Try
            Call Koneksi()
            Dim update As String = "update gate set status='close'"
            CMD = New MySqlCommand(update, Conn)
            CMD.ExecuteNonQuery()
        Catch ex As Exception

        End Try

    End Sub

    Sub statusgateopen()
        Try
            Call Koneksi()
            Dim update As String = "update gate set status='open'"
            CMD = New MySqlCommand(update, Conn)
            CMD.ExecuteNonQuery()
        Catch ex As Exception

        End Try

    End Sub

    Private Sub Button3_Click(sender As Object, e As EventArgs) Handles btnHIDE.Click
        If btnHIDE.Text = "H" Then
            pctLORONG1.Hide()
            pctLORONG2.Hide()
            btnHIDE.Text = "S"
        ElseIf btnHIDE.Text = "S" Then
            pctLORONG1.Show()
            pctLORONG2.Show()
            btnHIDE.Text = "H"
        End If
    End Sub

    Private Sub PictureBox54_Click(sender As Object, e As EventArgs) Handles PictureBox54.Click
        pctSPORT.Hide()
        pctMBIASA.Hide()
        pctMLAMA.Hide()
        pctMOBILPOL.Hide()
    End Sub

    Private Sub PictureBox42_Click(sender As Object, e As EventArgs) Handles PictureBox42.Click
        pctSPORT.Show()
        pctMBIASA.Show()
        pctMLAMA.Show()
        pctMOBILPOL.Show()
    End Sub


    Private Sub Timer2_Tick(sender As Object, e As EventArgs) Handles tmrETOLL.Tick
        Try
            pctETOLL.Left = pctETOLL.Left + 2
            If pctETOLL.Left >= 238 And pctETOLL.Left <= 248 Then
                tmrETOLL.Enabled = False
                tmrETOLL2.Enabled = True
            ElseIf pctETOLL.Left = 750 Then
                tmrPALANGETOLL2.Enabled = True
            ElseIf pctETOLL.Left = 1360 Then
                tmrETOLL.Enabled = False
                lblETOLL.Text = 0
            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles tmrWKTETOLL.Tick
        lblETOLL.Text = lblETOLL.Text + 1
        If lblETOLL.Text = 5 Then
            tmrWKTETOLL.Enabled = False
            tmrPALANGETOLL.Enabled = True
        End If
    End Sub


    Private Sub Button3_Click_1(sender As Object, e As EventArgs) Handles Button3.Click
        If txtPENGEMUDI.Text = "" Then
            MsgBox("Masukan Pengemudi Kedalam mobil terlebih dahulu!")
        Else
            Try
                lblETOLL.Text = 0
                pctMOBILAPT.Top = 326
                pctMOBILAPT.Left = 12
                pctETOLL.Top = 256
                pctETOLL.Left = 12
                tmrETOLL.Enabled = True
                tmrMOBILAPT.Enabled = True
                TmrCHECKDB.Enabled = False
                TmrCHECKDB.Enabled = True
            Catch ex As Exception

            End Try
        End If


    End Sub

    Private Sub tmrPALANGETOLL_Tick(sender As Object, e As EventArgs) Handles tmrPALANGETOLL.Tick
        Try
            pctPALANGETOLL.Height = pctPALANGETOLL.Height - 1
            If pctPALANGETOLL.Height = 20 Then
                tmrPALANGETOLL.Enabled = False
                pctLAMPUETOLL.BackColor = Color.GreenYellow
                tmrETOLL.Enabled = True
            End If
        Catch ex As Exception

        End Try
    End Sub

    Private Sub tmrPALANGETOLL2_Tick(sender As Object, e As EventArgs) Handles tmrPALANGETOLL2.Tick
        Try
            pctPALANGETOLL.Height = pctPALANGETOLL.Height + 1
            If pctPALANGETOLL.Height = 55 Then
                tmrPALANGETOLL2.Enabled = False
                pctLAMPUETOLL.BackColor = Color.Red
            End If

        Catch ex As Exception

        End Try
    End Sub

    Private Sub Button4_Click(sender As Object, e As EventArgs) Handles Button4.Click
        MsgBox("Pengemudi telah siap")
        saldoakun()
        saldoprovider()

    End Sub

    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        hargagate()
        grpMOBIL.Hide()

    End Sub

    Private Sub Button5_Click(sender As Object, e As EventArgs) Handles Button5.Click
        If txtPENGEMUDI.Text = "" Then
            MsgBox("Masukan Pengemudi Kedalam mobil terlebih dahulu!")
        Else
            tmrMOBILAPT.Enabled = True
            TmrCHECKDB.Enabled = True
            pctMOBILAPT.Top = 326
            pctMOBILAPT.Left = 12
            tmrMOBILAPT.Enabled = True
        End If

    End Sub

    Private Sub Button6_Click(sender As Object, e As EventArgs) Handles Button6.Click
        grpMOBIL.Hide()

    End Sub

    Private Sub Button7_Click(sender As Object, e As EventArgs) Handles Button7.Click
        grpMOBIL.Show()

    End Sub


    Private Sub PictureBox66_Click(sender As Object, e As EventArgs) Handles PictureBox66.Click
        Try
            TmrCHECKDB.Enabled = True
            statusgateopen()
        Catch ex As Exception

        End Try
    End Sub

    Private Sub tmrETOLL2_Tick(sender As Object, e As EventArgs) Handles tmrETOLL2.Tick
        pctETOLL.Left = pctETOLL.Left + 1
        If pctETOLL.Left = 592 Then
            tmrETOLL2.Enabled = False
            tmrWKTETOLL.Enabled = True

        End If
    End Sub
End Class
