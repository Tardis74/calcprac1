#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Генератор тестовых данных для задачи умножения матриц.
Создаёт два файла data1.dat и data2.dat в формате:
# n
a11 a12 ... a1n
a21 a22 ... a2n
...
"""

import random
import sys

def generate_matrix_file(filename, n):
    """Генерирует файл с матрицей n x n случайных чисел."""
    with open(filename, 'w') as f:
        # Заголовок
        f.write(f'# {n}\n')
        for i in range(n):
            # Генерируем строку из n случайных чисел (диапазон можно менять)
            row = [str(random.uniform(-10.0, 10.0)) for _ in range(n)]
            f.write(' '.join(row) + '\n')
    print(f"Файл {filename} с матрицей {n}x{n} создан.")

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Использование: python generate.py <размер матрицы>")
        sys.exit(1)
    try:
        n = int(sys.argv[1])
        if n <= 0:
            raise ValueError
    except ValueError:
        print("Размер должен быть положительным целым числом.")
        sys.exit(1)

    generate_matrix_file('data1.dat', n)
    generate_matrix_file('data2.dat', n)
