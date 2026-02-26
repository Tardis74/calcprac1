#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Программа верификации результата умножения матриц.
Сравнивает результат, полученный программой на Паскале (файл result.dat),
с эталонным умножением через numpy.
"""

import numpy as np
import sys

def read_matrix(filename):
    """Читает матрицу из файла в формате '# n' и строки чисел."""
    with open(filename, 'r') as f:
        # Первая строка: '# n'
        header = f.readline().strip()
        if not header.startswith('#'):
            raise ValueError(f"Файл {filename} не начинается с '#'")
        n = int(header[1:].strip())
        # Читаем остальные строки
        matrix = []
        for _ in range(n):
            line = f.readline()
            if not line:
                raise ValueError(f"Файл {filename} слишком короткий")
            # Разбиваем по пробелам и преобразуем в float
            row = list(map(float, line.split()))
            if len(row) != n:
                raise ValueError(f"Строка содержит {len(row)} чисел, ожидалось {n}")
            matrix.append(row)
        return np.array(matrix, dtype=np.float64)

def main():
    # Читаем матрицы A и B
    try:
        A = read_matrix('data1.dat')
        B = read_matrix('data2.dat')
    except Exception as e:
        print(f"Ошибка чтения входных файлов: {e}")
        sys.exit(1)

    # Проверка размеров
    if A.shape != B.shape or A.shape[0] != A.shape[1]:
        print("Матрицы должны быть квадратными и одинакового размера.")
        sys.exit(1)

    n = A.shape[0]
    print(f"Размер матриц: {n}x{n}")

    # Вычисляем эталонное произведение с помощью numpy (использует высокооптимизированные библиотеки BLAS)
    print("Вычисление эталонного произведения через numpy...")
    C_expected = np.dot(A, B)

    # Читаем результат Паскаля
    try:
        C_pascal = read_matrix('result.dat')
    except Exception as e:
        print(f"Ошибка чтения result.dat: {e}")
        sys.exit(1)

    if C_pascal.shape != (n, n):
        print(f"Размер результата Паскаля {C_pascal.shape} не совпадает с ожидаемым {n}x{n}")
        sys.exit(1)

    # Сравнение с учётом погрешности
    # Используем среднюю абсолютную разницу и максимальную разницу
    abs_diff = np.abs(C_pascal - C_expected)
    max_diff = np.max(abs_diff)
    mean_diff = np.mean(abs_diff)
    rel_diff = np.linalg.norm(C_pascal - C_expected) / np.linalg.norm(C_expected)  # относительная норма

    print(f"Максимальная абсолютная разница: {max_diff:.3e}")
    print(f"Средняя абсолютная разница: {mean_diff:.3e}")
    print(f"Относительная норма разницы: {rel_diff:.3e}")

    # Порог погрешности (можно настроить)
    tol = 1e-10
    if rel_diff < tol and max_diff < 1e-8:
        print("Результат совпадает с эталоном в пределах допустимой погрешности.")
    else:
        print("ВНИМАНИЕ: результат может быть неверным!")

    # Дополнительно можно вывести несколько элементов для наглядности
    print("\nНесколько элементов (первые 5x5):")
    print("Паскаль:")
    print(C_pascal[:5, :5])
    print("Numpy:")
    print(C_expected[:5, :5])

if __name__ == "__main__":
    main()
