unit matrix_utils;

{$mode objfpc}{$H+}	//настройка компилятора для работы с модулями
{$COPERATORS ON}	//разрешаем операторы +=, -= (оптимально, но не обязательно)

interface

type	//тип матрица как динамический двумерный массив чисел Double
	TMatrix = array of array of Double;

//функция умножения матриц
function MultiplyMatrices(const A, B: TMatrix): TMatrix;

implementation

uses
	Classes, SysUtils;	//модуль для многопоточности

const
	BLOCK_SIZE = 128;	//размер блока кэширования (экспериментально подобран)

type
	//поток-рабочий, обрабатывающий диапазон блоков строк
	PMatrix = ^TMatrix;
	TMatMulThread = class(TThread)
	private
		FA, FB, FC: PMatrix;	// указатели на матрицы
		FStartBlock, FEndBlock: Integer;	// диапазон блоков строк (номера блоков)
		Fn, FBlockSize: Integer;
	protected
		procedure Execute; override;
	public
	constructor Create(AA, AB, AC: PMatrix; AStartBlock, AEndBlock, An, ABlockSize: Integer);
  end;

constructor TMatMulThread.Create(AA, AB, AC: PMatrix; AStartBlock, AEndBlock, An, ABlockSize: Integer);
begin
	inherited Create(False);	//False = запустить сразу после создания
	FA := AA;
	FB := AB;
	FC := AC;
	FStartBlock := AStartBlock;
	FEndBlock := AEndBlock;
	Fn := An;
	FBlockSize := ABlockSize;
	FreeOnTerminate := False;	//чтобы потом можно было дождаться
end;

//потоковая процедура для обработки блока
procedure TMatMulThread.Execute;
var
	i, j, k: Integer;	//для циклов внутри блока
	jj, kk: Integer;	//внешние индексы для прохода по блокам
	i_start, i_end: Integer;	//начало и конец блока строк
	j_end, k_end: Integer;	//для столбцов
	blockIndex: Integer;       //счётчик блоков (номер текущего блока)
begin
	for blockIndex := FStartBlock to FEndBlock - 1 do
	begin
		//вычисляем диапазон строк, которые обрабатывает этот поток.
		i_start := blockIndex * FBlockSize;
		i_end := i_start + FBlockSize;
		if i_end > Fn then i_end := Fn;	//последний блок может быть неполным
		
		//внешний цикл по блокам столбцов матрицы
		jj := 0;
		while jj < Fn do
		begin
			j_end := jj + FBlockSize;
			if j_end > Fn then j_end := Fn;
			//обнуляем блок матрицы C, который будем вычислять.
			for i := i_start to i_end - 1 do
        			for j := jj to j_end - 1 do
          				FC^[i, j] := 0.0;
          				
          		//внутренний цикл по блокам общего измерения
			kk := 0;
     			while kk < Fn do
      			begin
				k_end := kk + FBlockSize;
				if k_end > Fn then k_end := Fn;

				//самые внутренний цикл: умножение двух подблоков
				for i := i_start to i_end - 1 do
					for k := kk to k_end - 1 do
						for j := jj to j_end - 1 do
							FC^[i, j] := FC^[i, j] + FA^[i, k] * FB^[k, j];
				kk := kk + FBlockSize;	//переходим к следующему блоку по k
			end;
			jj := jj + FBlockSize;	//переходим к следующему блоку по j
		end;
	end;
end;

//основная внешняя функция
function MultiplyMatrices(const A, B: TMatrix): TMatrix;
var
	n: Integer;
	numBlocks, i: Integer;
	threads: array of TMatMulThread;
	blocksPerThread, remainder: Integer;
	startBlock, endBlock: Integer;
	threadCount: Integer;
begin
	n := Length(A);
	if (n = 0) or (Length(B) <> n) then
		raise Exception.Create('У матриц отличаются размеры');
	
	SetLength(Result, n, n);

	//определяем количество потоков
	threadCount := GetCPUCount;
	if threadCount < 1 then threadCount := 1;
	threadCount := 8;

	numBlocks := (n + BLOCK_SIZE - 1) div BLOCK_SIZE;
	if numBlocks < threadCount then threadCount := numBlocks;	//не больше блоков

	//распределяем блоки по потокам
	SetLength(threads, threadCount);
	blocksPerThread := numBlocks div threadCount;
	remainder := numBlocks mod threadCount;
	startBlock := 0;

	for i := 0 to threadCount - 1 do
	begin
		endBlock := startBlock + blocksPerThread;
		if i < remainder then Inc(endBlock);	//первые потоки получают на блок больше
		threads[i] := TMatMulThread.Create(@A, @B, @Result, startBlock, endBlock, n, BLOCK_SIZE);
		startBlock := endBlock;
	end;

	//ожидаем завершения всех потоков
	for i := 0 to threadCount - 1 do
	begin
		threads[i].WaitFor;
		threads[i].Free;
	end;
end;

end.
