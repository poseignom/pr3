program LegacyCSVGenerator;

{$mode objfpc}{$H+}

uses
  SysUtils, Classes, DateUtils, StrUtils, Process;

const
  VERSION = '2.0.0';
  LOG_FILE = '/data/logs/legacy_%s.log';
  OUTPUT_DIR = '/data/csv';
  XSLX_OUTPUT_DIR = '/data/xlsx';

type
  TDataFormat = (dfTimestamp, dfBoolean, dfNumber, dfString);
  
  TDataRow = record
    Timestamp: TDateTime;
    IsActive: Boolean;
    Value: Double;
    Description: String;
    Code: Integer;
    Status: String;
    Metadata: String;
  end;

// Логирование
procedure Log(Message: String; Level: String = 'INFO');
var
  LogFile: TextFile;
  LogPath: String;
begin
  LogPath := Format(LOG_FILE, [FormatDateTime('yyyy-mm-dd', Now)]);
  
  AssignFile(LogFile, LogPath);
  try
    if FileExists(LogPath) then
      Append(LogFile)
    else
      Rewrite(LogFile);
    
    WriteLn(LogFile, Format('%s [%s] %s', [
      FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Now),
      Level,
      Message
    ]));
  finally
    CloseFile(LogFile);
  end;
  
  WriteLn(Format('[%s] %s', [Level, Message]));
end;

// Генерация тестовых данных
function GenerateTestData(Count: Integer): TDataRow;
var
  Row: TDataRow;
begin
  Row.Timestamp := Now - Random(86400) / 86400;
  Row.IsActive := Random(2) = 1;
  Row.Value := 1000 + Random(5000) / 100;
  Row.Description := 'Тестовая запись #' + IntToStr(Random(10000));
  Row.Code := 1000 + Random(9000);
  
  case Random(3) of
    0: Row.Status := 'НОРМА';
    1: Row.Status := 'ПРЕДУПРЕЖДЕНИЕ';
    2: Row.Status := 'ОШИБКА';
  end;
  
  Row.Metadata := Format('{"sensor": "S%d", "unit": "V", "quality": %d}', 
    [Random(10), Random(100)]);
    
  Result := Row;
end;

// Форматирование значений по типу
function FormatValue(Value: Variant; FormatType: TDataFormat): String;
begin
  case FormatType of
    dfTimestamp:
      Result := FormatDateTime('yyyy-mm-dd hh:nn:ss.zzz', Value);
    dfBoolean:
      if Value then Result := 'ИСТИНА' else Result := 'ЛОЖЬ';
    dfNumber:
      Result := FormatFloat('0.00', Value);
    dfString:
      begin
        Result := String(Value);
        // Экранирование кавычек и разделителей
        if (Pos('"', Result) > 0) or (Pos(',', Result) > 0) or (Pos(';', Result) > 0) then
          Result := '"' + StringReplace(Result, '"', '""', [rfReplaceAll]) + '"';
      end;
  end;
end;

// Генерация CSV файла
function GenerateCSV(Count: Integer): String;
var
  CSVFile: TextFile;
  FileName: String;
  I: Integer;
  Row: TDataRow;
begin
  // Создаем директорию если не существует
  if not DirectoryExists(OUTPUT_DIR) then
    CreateDir(OUTPUT_DIR);
  
  FileName := OUTPUT_DIR + '/telemetry_' + 
    FormatDateTime('yyyy-mm-dd_hh-nn-ss', Now) + '.csv';
  
  AssignFile(CSVFile, FileName);
  try
    Rewrite(CSVFile);
    
    // Заголовок с типами данных
    WriteLn(CSVFile, 'Timestamp;IsActive;Value;Description;Code;Status;Metadata');
    WriteLn(CSVFile, 'DATETIME;BOOLEAN;DECIMAL(10,2);TEXT;INTEGER;ENUM;JSON');
    
    // Данные
    for I := 1 to Count do
    begin
      Row := GenerateTestData(I);
      
      WriteLn(CSVFile, 
        FormatValue(Row.Timestamp, dfTimestamp) + ';' +
        FormatValue(Row.IsActive, dfBoolean) + ';' +
        FormatValue(Row.Value, dfNumber) + ';' +
        FormatValue(Row.Description, dfString) + ';' +
        IntToStr(Row.Code) + ';' +
        FormatValue(Row.Status, dfString) + ';' +
        FormatValue(Row.Metadata, dfString)
      );
    end;
    
    Result := FileName;
    Log(Format('Сгенерирован CSV: %s (%d записей)', [FileName, Count]));
  finally
    CloseFile(CSVFile);
  end;
end;

// Создание визуализации HTML
procedure CreateHTMLVisualization(CSVFile: String);
var
  HTMLFile: TextFile;
  HTMLFileName: String;
begin
  HTMLFileName := ChangeFileExt(CSVFile, '.html');
  
  AssignFile(HTMLFile, HTMLFileName);
  try
    Rewrite(HTMLFile);
    
    WriteLn(HTMLFile, '<!DOCTYPE html>');
    WriteLn(HTMLFile, '<html lang="ru">');
    WriteLn(HTMLFile, '<head>');
    WriteLn(HTMLFile, '    <meta charset="UTF-8">');
    WriteLn(HTMLFile, '    <meta name="viewport" content="width=device-width, initial-scale=1.0">');
    WriteLn(HTMLFile, '    <title>Визуализация CSV данных</title>');
    WriteLn(HTMLFile, '    <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.1.3/dist/css/bootstrap.min.css" rel="stylesheet">');
    WriteLn(HTMLFile, '    <link href="https://cdn.datatables.net/1.11.5/css/jquery.dataTables.min.css" rel="stylesheet">');
    WriteLn(HTMLFile, '    <style>');
    WriteLn(HTMLFile, '        body { padding: 20px; background: #f8f9fa; }');
    WriteLn(HTMLFile, '        .card { margin-bottom: 20px; }');
    WriteLn(HTMLFile, '        .table-container { background: white; border-radius: 10px; padding: 20px; }');
    WriteLn(HTMLFile, '        .true { background-color: #d4edda !important; }');
    WriteLn(HTMLFile, '        .false { background-color: #f8d7da !important; }');
    WriteLn(HTMLFile, '        .status-normal { background-color: #d4edda; }');
    WriteLn(HTMLFile, '        .status-warning { background-color: #fff3cd; }');
    WriteLn(HTMLFile, '        .status-error { background-color: #f8d7da; }');
    WriteLn(HTMLFile, '        .number { text-align: right; font-family: monospace; }');
    WriteLn(HTMLFile, '    </style>');
    WriteLn(HTMLFile, '</head>');
    WriteLn(HTMLFile, '<body>');
    WriteLn(HTMLFile, '    <div class="container">');
    WriteLn(HTMLFile, '        <div class="card">');
    WriteLn(HTMLFile, '            <div class="card-header">');
    WriteLn(HTMLFile, '                <h4 class="mb-0">Визуализация CSV данных</h4>');
    WriteLn(HTMLFile, '                <small class="text-muted">Файл: ' + ExtractFileName(CSVFile) + '</small>');
    WriteLn(HTMLFile, '            </div>');
    WriteLn(HTMLFile, '            <div class="card-body">');
    WriteLn(HTMLFile, '                <div class="table-responsive">');
    WriteLn(HTMLFile, '                    <table id="csvTable" class="table table-striped table-bordered">');
    WriteLn(HTMLFile, '                        <thead>');
    WriteLn(HTMLFile, '                            <tr>');
    WriteLn(HTMLFile, '                                <th>Timestamp</th>');
    WriteLn(HTMLFile, '                                <th>IsActive</th>');
    WriteLn(HTMLFile, '                                <th>Value</th>');
    WriteLn(HTMLFile, '                                <th>Description</th>');
    WriteLn(HTMLFile, '                                <th>Code</th>');
    WriteLn(HTMLFile, '                                <th>Status</th>');
    WriteLn(HTMLFile, '                                <th>Metadata</th>');
    WriteLn(HTMLFile, '                            </tr>');
    WriteLn(HTMLFile, '                        </thead>');
    WriteLn(HTMLFile, '                        <tbody>');
    WriteLn(HTMLFile, '                            <!-- Данные будут загружены через JavaScript -->');
    WriteLn(HTMLFile, '                        </tbody>');
    WriteLn(HTMLFile, '                    </table>');
    WriteLn(HTMLFile, '                </div>');
    WriteLn(HTMLFile, '            </div>');
    WriteLn(HTMLFile, '        </div>');
    WriteLn(HTMLFile, '    </div>');
    WriteLn(HTMLFile, '    ');
    WriteLn(HTMLFile, '    <script src="https://code.jquery.com/jquery-3.6.0.min.js"></script>');
    WriteLn(HTMLFile, '    <script src="https://cdn.datatables.net/1.11.5/js/jquery.dataTables.min.js"></script>');
    WriteLn(HTMLFile, '    <script>');
    WriteLn(HTMLFile, '        $(document).ready(function() {');
    WriteLn(HTMLFile, '            $.get("' + ExtractFileName(CSVFile) + '", function(csv) {');
    WriteLn(HTMLFile, '                const rows = csv.split("\\n");');
    WriteLn(HTMLFile, '                const tbody = $("#csvTable tbody");');
    WriteLn(HTMLFile, '                ');
    WriteLn(HTMLFile, '                // Пропускаем заголовок с типами');
    WriteLn(HTMLFile, '                for(let i = 2; i < rows.length; i++) {');
    WriteLn(HTMLFile, '                    if(rows[i].trim() === "") continue;');
    WriteLn(HTMLFile, '                    ');
    WriteLn(HTMLFile, '                    const cells = rows[i].split(";");');
    WriteLn(HTMLFile, '                    if(cells.length >= 7) {');
    WriteLn(HTMLFile, '                        const isActive = cells[1] === "ИСТИНА";');
    WriteLn(HTMLFile, '                        const status = cells[5] || "";');
    WriteLn(HTMLFile, '                        ');
    WriteLn(HTMLFile, '                        let rowClass = isActive ? "true" : "false";');
    WriteLn(HTMLFile, '                        if(status === "НОРМА") rowClass += " status-normal";');
    WriteLn(HTMLFile, '                        if(status === "ПРЕДУПРЕЖДЕНИЕ") rowClass += " status-warning";');
    WriteLn(HTMLFile, '                        if(status === "ОШИБКА") rowClass += " status-error";');
    WriteLn(HTMLFile, '                        ');
    WriteLn(HTMLFile, '                        const row = `');
    WriteLn(HTMLFile, '                            <tr class="${rowClass}">');
    WriteLn(HTMLFile, '                                <td>${cells[0]}</td>');
    WriteLn(HTMLFile, '                                <td><span class="badge ${isActive ? "bg-success" : "bg-danger"}">${cells[1]}</span></td>');
    WriteLn(HTMLFile, '                                <td class="number">${parseFloat(cells[2]).toFixed(2)}</td>');
    WriteLn(HTMLFile, '                                <td>${cells[3].replace(/"/g, "")}</td>');
    WriteLn(HTMLFile, '                                <td>${cells[4]}</td>');
    WriteLn(HTMLFile, '                                <td><span class="badge bg-${status === "НОРМА" ? "success" : status === "ПРЕДУПРЕЖДЕНИЕ" ? "warning" : "danger"}">${status}</span></td>');
    WriteLn(HTMLFile, '                                <td><small>${cells[6]}</small></td>');
    WriteLn(HTMLFile, '                            </tr>');
    WriteLn(HTMLFile, '                        `;');
    WriteLn(HTMLFile, '                        ');
    WriteLn(HTMLFile, '                        tbody.append(row);');
    WriteLn(HTMLFile, '                    }');
    WriteLn(HTMLFile, '                }');
    WriteLn(HTMLFile, '                ');
    WriteLn(HTMLFile, '                $("#csvTable").DataTable({');
    WriteLn(HTMLFile, '                    pageLength: 25,');
    WriteLn(HTMLFile, '                    order: [[0, "desc"]],');
    WriteLn(HTMLFile, '                    language: {');
    WriteLn(HTMLFile, '                        url: "//cdn.datatables.net/plug-ins/1.11.5/i18n/ru.json"');
    WriteLn(HTMLFile, '                    }');
    WriteLn(HTMLFile, '                });');
    WriteLn(HTMLFile, '            }).fail(function() {');
    WriteLn(HTMLFile, '                tbody.html("<tr><td colspan=\'7\' class=\'text-center text-danger\'>Ошибка загрузки CSV файла</td></tr>");');
    WriteLn(HTMLFile, '            });');
    WriteLn(HTMLFile, '        });');
    WriteLn(HTMLFile, '    </script>');
    WriteLn(HTMLFile, '</body>');
    WriteLn(HTMLFile, '</html>');
    
    Log('Создана HTML визуализация: ' + HTMLFileName);
  finally
    CloseFile(HTMLFile);
  end;
end;

// Конвертация CSV в XLSX через Python
procedure ConvertToXLSX(CSVFile: String);
var
  PythonScript: TStringList;
  ScriptFile: String;
  OutputFile: String;
begin
  if not DirectoryExists(XSLX_OUTPUT_DIR) then
    CreateDir(XSLX_OUTPUT_DIR);
  
  OutputFile := XSLX_OUTPUT_DIR + '/' + 
    ChangeFileExt(ExtractFileName(CSVFile), '.xlsx');
  
  PythonScript := TStringList.Create;
  try
    PythonScript.Add('import pandas as pd');
    PythonScript.Add('import json');
    PythonScript.Add('from datetime import datetime');
    PythonScript.Add('');
    PythonScript.Add('def convert_csv_to_xlsx():');
    PythonScript.Add('    try:');
    PythonScript.Add('        # Чтение CSV');
    PythonScript.Add('        df = pd.read_csv("' + CSVFile + '", sep=";", skiprows=[1])');
    PythonScript.Add('        ');
    PythonScript.Add('        # Форматирование колонок');
    PythonScript.Add('        if "Timestamp" in df.columns:');
    PythonScript.Add('            df["Timestamp"] = pd.to_datetime(df["Timestamp"])');
    PythonScript.Add('        ');
    PythonScript.Add('        if "Value" in df.columns:');
    PythonScript.Add('            df["Value"] = df["Value"].round(2)');
    PythonScript.Add('        ');
    PythonScript.Add('        if "Code" in df.columns:');
    PythonScript.Add('            df["Code"] = df["Code"].astype(int)');
    PythonScript.Add('        ');
    PythonScript.Add('        # Создание Excel файла с форматированием');
    PythonScript.Add('        with pd.ExcelWriter("' + OutputFile + '", engine="openpyxl") as writer:');
    PythonScript.Add('            df.to_excel(writer, sheet_name="Данные", index=False)');
    PythonScript.Add('            ');
    PythonScript.Add('            workbook = writer.book');
    PythonScript.Add('            worksheet = writer.sheets["Данные"]');
    PythonScript.Add('            ');
    PythonScript.Add('            # Форматирование заголовков');
    PythonScript.Add('            header_font = Font(bold=True, color="FFFFFF")');
    PythonScript.Add('            header_fill = PatternFill(start_color="2E86C1", end_color="2E86C1", fill_type="solid")');
    PythonScript.Add('            ');
    PythonScript.Add('            for cell in worksheet[1]:');
    PythonScript.Add('                cell.font = header_font');
    PythonScript.Add('                cell.fill = header_fill');
    PythonScript.Add('            ');
    PythonScript.Add('            # Автоширина колонок');
    PythonScript.Add('            for column in worksheet.columns:');
    PythonScript.Add('                max_length = 0');
    PythonScript.Add('                column_letter = column[0].column_letter');
    PythonScript.Add('                for cell in column:');
    PythonScript.Add('                    try:');
    PythonScript.Add('                        if len(str(cell.value)) > max_length:');
    PythonScript.Add('                            max_length = len(str(cell.value))');
    PythonScript.Add('                    except:');
    PythonScript.Add('                        pass');
    PythonScript.Add('                adjusted_width = min(max_length + 2, 50)');
    PythonScript.Add('                worksheet.column_dimensions[column_letter].width = adjusted_width');
    PythonScript.Add('            ');
    PythonScript.Add('        print(f"Создан XLSX файл: {output_file}")');
    PythonScript.Add('        return True');
    PythonScript.Add('    except Exception as e:');
    PythonScript.Add('        print(f"Ошибка: {e}")');
    PythonScript.Add('        return False');
    PythonScript.Add('');
    PythonScript.Add('if __name__ == "__main__":');
    PythonScript.Add('    convert_csv_to_xlsx()');
    
    ScriptFile := GetTempDir + 'convert_to_xlsx.py';
    PythonScript.SaveToFile(ScriptFile);
    
    Log('Запуск конвертации CSV в XLSX...');
    
    // Запуск Python скрипта
    if RunCommand('python3', [ScriptFile], nil) then
      Log('Успешно создан XLSX файл: ' + OutputFile)
    else
      Log('Ошибка при создании XLSX файла', 'ERROR');
      
  finally
    PythonScript.Free;
  end;
end;

// Основная программа
var
  Period: Integer;
  CSVFile: String;
begin
  Randomize;
  
  Log('=== Запуск Legacy CSV Generator ' + VERSION + ' ===');
  Log('Время запуска: ' + FormatDateTime('yyyy-mm-dd hh:nn:ss', Now));
  
  try
    // Получаем период из переменных окружения
    Period := StrToIntDef(GetEnvironmentVariable('GEN_PERIOD_SEC'), 300);
    Log('Период генерации: ' + IntToStr(Period) + ' сек');
    
    while True do
    begin
      try
        Log('Начало цикла генерации');
        
        // Генерация CSV
        CSVFile := GenerateCSV(100);
        
        // Создание HTML визуализации
        CreateHTMLVisualization(CSVFile);
        
        // Конвертация в XLSX
        ConvertToXLSX(CSVFile);
        
        Log('Цикл завершен успешно');
        
      except
        on E: Exception do
          Log('Ошибка в цикле: ' + E.Message, 'ERROR');
      end;
      
      // Ожидание перед следующим циклом
      Sleep(Period * 1000);
    end;
    
  except
    on E: Exception do
    begin
      Log('Критическая ошибка: ' + E.Message, 'ERROR');
      Halt(1);
    end;
  end;
end.