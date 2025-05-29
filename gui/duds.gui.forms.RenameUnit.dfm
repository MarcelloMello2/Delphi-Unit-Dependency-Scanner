object frmRenameUnitName: TfrmRenameUnitName
  Left = 0
  Top = 0
  ActiveControl = edtNewName
  AutoSize = True
  BorderStyle = bsDialog
  Caption = 'Rename Unit'
  ClientHeight = 332
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  TextHeight = 13
  object infoRenameCSV: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 176
    Width = 440
    Height = 153
    Align = alTop
    BevelOuter = bvNone
    Color = clGray
    ParentBackground = False
    TabOrder = 0
    Visible = False
    ExplicitTop = 177
    object Panel5: TPanel
      AlignWithMargins = True
      Left = 1
      Top = 1
      Width = 438
      Height = 151
      Margins.Left = 1
      Margins.Top = 1
      Margins.Right = 1
      Margins.Bottom = 1
      Align = alClient
      BevelOuter = bvNone
      Color = 12910591
      ParentBackground = False
      TabOrder = 0
      ExplicitHeight = 129
      object Label2: TLabel
        AlignWithMargins = True
        Left = 1
        Top = 1
        Width = 436
        Height = 149
        Margins.Left = 1
        Margins.Top = 1
        Margins.Right = 1
        Margins.Bottom = 1
        Align = alClient
        Caption = 
          'Formato esperado no arquivo CSV:'#13#10#13#10'NomeDaUnidadeAntiga1;NomeDaN' +
          'ovaUnidade1'#13#10'NomeDaUnidadeAntiga2;NomeDaNovaUnidade2'#13#10'NomeDaUnid' +
          'adeAntiga3;NomeDaNovaUnidade3'#13#10'...'#13#10#13#10'Sem cabe'#231'alho CSV. N'#227'o adi' +
          'cione ".pas" aos nomes das unidades, nem defina os caminhos.'#13#10'So' +
          'mente nomes das unidades.'
        Color = 14548991
        Font.Charset = ANSI_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Segoe UI'
        Font.Style = []
        ParentColor = False
        ParentFont = False
        Transparent = True
        WordWrap = True
        ExplicitWidth = 417
        ExplicitHeight = 130
      end
    end
  end
  object pnlButtons: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 135
    Width = 440
    Height = 35
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
    object btnOK: TButton
      Left = 142
      Top = 8
      Width = 75
      Height = 25
      Caption = '&OK'
      Default = True
      Enabled = False
      ModalResult = 1
      TabOrder = 0
    end
    object btnCancel: TButton
      Left = 222
      Top = 8
      Width = 75
      Height = 25
      Cancel = True
      Caption = '&Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
  object pnlMain: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 440
    Height = 126
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      440
      126)
    object Label1: TLabel
      Left = 8
      Top = 7
      Width = 83
      Height = 13
      Caption = 'New Unit Name:'
    end
    object chkRenameLowerCaseExtension: TCheckBox
      Left = 8
      Top = 108
      Width = 361
      Height = 17
      Caption = 'Extens'#227'o em min'#250'sculas, por ex. ".Pas" => ".pas"'
      Checked = True
      State = cbChecked
      TabOrder = 0
    end
    object chkInsertOldNameComment: TCheckBox
      Left = 8
      Top = 87
      Width = 425
      Height = 17
      Caption = 'Inserir coment'#225'rio "{renomeado de xyz.pas}"'
      Checked = True
      State = cbChecked
      TabOrder = 1
    end
    object chkRenameHistoryFiles: TCheckBox
      Left = 8
      Top = 64
      Width = 237
      Height = 17
      Caption = 'Renomear arquivos na pasta "__history"'
      Checked = True
      State = cbChecked
      TabOrder = 2
    end
    object chkDummyRun: TCheckBox
      Left = 8
      Top = 44
      Width = 329
      Height = 17
      Caption = 'Dummy Run - Nenhum arquivo ser'#225' atualizado'
      TabOrder = 3
    end
    object edtNewName: TEdit
      Left = 8
      Top = 21
      Width = 432
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 4
      OnChange = edtNewNameChange
      OnKeyPress = edtNewNameKeyPress
    end
  end
end
