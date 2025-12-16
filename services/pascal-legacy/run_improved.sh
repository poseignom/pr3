#!/usr/bin/env bash
set -e

echo "=== Legacy CSV Generator ==="
echo "Version: 2.0.0"
echo "Start time: $(date)"
echo "Environment:"
echo "  GEN_PERIOD_SEC: ${GEN_PERIOD_SEC}"
echo "  CSV_OUT_DIR: ${CSV_OUT_DIR}"
echo "  LOG_DIR: ${LOG_DIR}"

# Создание директорий
mkdir -p ${CSV_OUT_DIR} ${LOG_DIR}

# Запуск Pascal программы
echo "Starting Pascal generator..."
exec /app/legacy_generator