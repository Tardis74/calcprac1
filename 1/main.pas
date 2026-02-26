program MatrixMultiplication;

{$mode objfpc}{$H+}
{$COPERATORS ON}

uses
	cthreads,
	SysUtils,	//модуль для работы с системой: файлы, исключения, строки
	DateUtils,	//модуль для работы с датой/временем (замеры)
	matrix_utils; 
//парсер строк
procedure ParseLine(const line: string; var row: array of Double);
var
	i, idx, start: Integer;	//текущая позиция, индекс в массиве row, начало очередного числа
	n: Integer;	//ожидаемое количество чисел
	value: Double;	//временное значение числа
	code: Integer;	//код ошибки при преобразовании значения если произойдёт
begin
	n := Length(row);	//определяем, сколько чисел должно быть
	i := 1;		//начинаем с первого символа
	idx := 0;
	while (i <= Length(line)) and (idx < n) do //пока не конец строки и не собрали все числа
	begin
		//пропускаем пробелы
		while (i <= Length(line)) and (line[i] = ' ') do
			Inc(i);
			if i > Length(line) then
				Break;	//если дошли до конца, выходим
		start := i;	//запоминаем начало числа
		//идём до следующего пробела или конца строки
		while (i <= Length(line)) and (line[i] <> ' ') do
			Inc(i);
			//извлекаем подстроку, содержащую число, и преобразуем в Double
			Val(Copy(line, start, i - start), value, code);
			if code <> 0 then
				raise Exception.Create('Неправильное число на позиции ' + IntToStr(start));
		row[idx] := value;	//сохраняем в массив
		Inc(idx); 	// увеличиваем индекс
	end;
	if idx <> n then	//если прочитали меньше чисел, чем ожидалось
		raise Exception.Create('Меньше чисел, чем ожидалось');
end;

//функция чтения матрицы из текстового файла с заданным именем
function ReadMatrixFromFile(const FileName: string): TMatrix;
var
	f: Text;	//текстовая файловая переменная
	line: string;	//строка для чтения
	n, i: Integer;	//размер матрицы и счётчик строк
	code: Integer;	//код ошибки при преобразовании размера
begin
	Assign(f, FileName);	//связывание файловой переменной с именем файла
	Reset(f);	//открываем файл для чтения
	try
		ReadLn(f, line);	//читаем первую строку
		line := Trim(line);	//удаляем лишние пробелы в начале и конце
		if (Length(line) < 2) or (line[1] <> '#') then
			raise Exception.Create('Первая строка должна начинаться с символа "#"');
		//удаляем первый символ '#' и пробелы после него
		Delete(line, 1, 1);
		line := Trim(line);
		Val(line, n, code);
		
		if (code <> 0) or (n <= 0) then
			raise Exception.Create('Размер матрицы неправильный!');
		SetLength(Result, n, n);	//выделяем память под матрицу n x n	
		
		// Читаем n строк матрицы
		for i := 0 to n-1 do
		begin
			if Eof(f) then
				raise Exception.Create('Файл неожиданно закончился');
			ReadLn(f, line);
			ParseLine(line, Result[i]);
		end;
	finally
		Close(f);
	end;
end;

//процедура записи матрицы в файл в требуемом формате
procedure WriteMatrixToFile(const FileName: string; const M: TMatrix);
var
	f: Text;
	n, i, j: Integer;
begin
	n := Length(M);
	Assign(f, FileName);
	Rewrite(f); 	//создаём файл для записи если не существует
	try
		WriteLn(f, '# ', n);	//заголовок
		for i := 0 to n-1 do
		begin
			for j := 0 to n-1 do
			begin
				Write(f, M[i, j]); //записываем число
				if j < n-1 then
					Write(f, ' ');	//разделяем пробелами, кроме последнего
			end;
			WriteLn(f);	//переход на новую строку
		end;
	finally
		Close(f);
	end;
end;

var
	A, B, C: TMatrix;	//переменные для матриц
	StartTime, EndTime: TDateTime;	//для замера времени
	seconds: Double;	//время в секундах
begin
	try
		Writeln('Читаю матрицу A из data1.dat...');
		A := ReadMatrixFromFile('data1.dat');
		Writeln('Размер матрицы A: ', Length(A), 'x', Length(A));
		
		Writeln('Читаю матрицу B из data2.dat...');
		B := ReadMatrixFromFile('data2.dat');
		
		if Length(B) <> Length(A) then
			raise Exception.Create('Размер матриц то не совпадает');
		StartTime := Now;	// запоминаем текущее время
		C := MultiplyMatrices(A, B);
		
		EndTime := Now;
		seconds := SecondSpan(EndTime, StartTime);
		
		Writeln('Время выполнения ', seconds:0:2, ' секунд.');
		
		Writeln('Запись результата в result.dat...');
		WriteMatrixToFile('result.dat', C);
	except
		on E: Exception do
			Writeln(E.Message);
	end;
	
end.
