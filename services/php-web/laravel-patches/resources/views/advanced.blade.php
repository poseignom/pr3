@extends('layouts.app')

@section('title', 'Расширенный дашборд')

@section('content')
<div class="animate-fade-in">
    <!-- Фильтры -->
    <div class="card glass-effect mb-4">
        <div class="card-body">
            <h5 class="card-title mb-3">
                <i class="fas fa-filter me-2"></i>Фильтры данных
            </h5>
            
            <form id="dashboardFilters" class="row g-3">
                <div class="col-md-3">
                    <label class="form-label">Дата с:</label>
                    <input type="datetime-local" class="form-control" id="dateFrom" 
                           value="{{ date('Y-m-d\T00:00', strtotime('-7 days')) }}">
                </div>
                
                <div class="col-md-3">
                    <label class="form-label">Дата по:</label>
                    <input type="datetime-local" class="form-control" id="dateTo" 
                           value="{{ date('Y-m-d\T23:59') }}">
                </div>
                
                <div class="col-md-2">
                    <label class="form-label">Сортировка:</label>
                    <select class="form-select" id="sortOrder">
                        <option value="asc">По возрастанию</option>
                        <option value="desc" selected>По убыванию</option>
                    </select>
                </div>
                
                <div class="col-md-2">
                    <label class="form-label">Столбец:</label>
                    <select class="form-select" id="sortColumn">
                        <option value="fetched_at">Дата</option>
                        <option value="altitude">Высота</option>
                        <option value="velocity">Скорость</option>
                        <option value="temperature">Температура</option>
                    </select>
                </div>
                
                <div class="col-md-2">
                    <label class="form-label">Лимит:</label>
                    <select class="form-select" id="limit">
                        <option value="10">10</option>
                        <option value="25" selected>25</option>
                        <option value="50">50</option>
                        <option value="100">100</option>
                    </select>
                </div>
                
                <div class="col-12">
                    <div class="input-group">
                        <input type="text" class="form-control" 
                               placeholder="Поиск по ключевым словам..." 
                               id="keywordSearch">
                        <button class="btn btn-primary" type="button" id="applyFilters">
                            <i class="fas fa-search me-1"></i>Применить
                        </button>
                        <button class="btn btn-outline-secondary" type="button" id="resetFilters">
                            <i class="fas fa-redo me-1"></i>Сбросить
                        </button>
                    </div>
                </div>
            </form>
        </div>
    </div>
    
    <!-- Метрики -->
    <div class="row mb-4">
        <div class="col-md-3">
            <div class="card metric-card animate-slide-left">
                <div class="card-body">
                    <div class="metric-value" id="totalRecords">0</div>
                    <div class="metric-label">Всего записей</div>
                </div>
            </div>
        </div>
        <div class="col-md-3">
            <div class="card metric-card animate-slide-left" style="animation-delay: 0.1s">
                <div class="card-body">
                    <div class="metric-value" id="avgAltitude">0</div>
                    <div class="metric-label">Средняя высота</div>
                </div>
            </div>
        </div>
        <div class="col-md-3">
            <div class="card metric-card animate-slide-left" style="animation-delay: 0.2s">
                <div class="card-body">
                    <div class="metric-value" id="maxVelocity">0</div>
                    <div class="metric-label">Макс. скорость</div>
                </div>
            </div>
        </div>
        <div class="col-md-3">
            <div class="card metric-card animate-slide-left" style="animation-delay: 0.3s">
                <div class="card-body">
                    <div class="metric-value" id="lastUpdate">-</div>
                    <div class="metric-label">Последнее обновление</div>
                </div>
            </div>
        </div>
    </div>
    
    <!-- Графики -->
    <div class="row mb-4">
        <div class="col-md-6">
            <div class="card">
                <div class="card-body">
                    <h5 class="card-title">Температура МКС</h5>
                    <canvas id="temperatureChart" height="200"></canvas>
                </div>
            </div>
        </div>
        <div class="col-md-6">
            <div class="card">
                <div class="card-body">
                    <h5 class="card-title">Высота орбиты</h5>
                    <canvas id="altitudeChart" height="200"></canvas>
                </div>
            </div>
        </div>
    </div>
    
    <!-- Таблица данных -->
    <div class="card">
        <div class="card-body">
            <div class="d-flex justify-content-between align-items-center mb-3">
                <h5 class="card-title m-0">Данные МКС</h5>
                <div>
                    <button class="btn btn-sm btn-outline-primary me-2" id="exportCSV">
                        <i class="fas fa-file-csv me-1"></i>CSV
                    </button>
                    <button class="btn btn-sm btn-outline-success" id="exportJSON">
                        <i class="fas fa-file-code me-1"></i>JSON
                    </button>
                </div>
            </div>
            
            <div class="table-responsive">
                <table class="table table-hover" id="issDataTable">
                    <thead>
                        <tr>
                            <th>Время (UTC)</th>
                            <th>Широта</th>
                            <th>Долгота</th>
                            <th>Высота (км)</th>
                            <th>Скорость (км/ч)</th>
                            <th>Температура (°C)</th>
                            <th>Статус</th>
                            <th>Действия</th>
                        </tr>
                    </thead>
                    <tbody id="issDataBody">
                        <!-- Данные загрузятся через AJAX -->
                    </tbody>
                </table>
            </div>
            
            <nav aria-label="Навигация">
                <ul class="pagination justify-content-center" id="pagination">
                    <!-- Пагинация -->
                </ul>
            </nav>
        </div>
    </div>
</div>

@push('scripts')
<script>
// Инициализация графиков
let temperatureChart = null;
let altitudeChart = null;

function initCharts() {
    const tempCtx = document.getElementById('temperatureChart').getContext('2d');
    temperatureChart = new Chart(tempCtx, {
        type: 'line',
        data: {
            labels: [],
            datasets: [{
                label: 'Температура (°C)',
                data: [],
                borderColor: 'rgb(255, 99, 132)',
                backgroundColor: 'rgba(255, 99, 132, 0.1)',
                tension: 0.4,
                fill: true
            }]
        },
        options: {
            responsive: true,
            plugins: {
                legend: { display: true },
                tooltip: { mode: 'index', intersect: false }
            },
            scales: {
                x: { display: true, title: { display: true, text: 'Время' } },
                y: { display: true, title: { display: true, text: 'Температура (°C)' } }
            }
        }
    });

    const altCtx = document.getElementById('altitudeChart').getContext('2d');
    altitudeChart = new Chart(altCtx, {
        type: 'bar',
        data: {
            labels: [],
            datasets: [{
                label: 'Высота (км)',
                data: [],
                backgroundColor: 'rgba(54, 162, 235, 0.7)',
                borderColor: 'rgb(54, 162, 235)',
                borderWidth: 1
            }]
        },
        options: {
            responsive: true,
            plugins: {
                legend: { display: true }
            },
            scales: {
                x: { display: true, title: { display: true, text: 'Время' } },
                y: { display: true, title: { display: true, text: 'Высота (км)' } }
            }
        }
    });
}

// Загрузка данных с фильтрами
async function loadData(page = 1) {
    const filters = {
        date_from: $('#dateFrom').val(),
        date_to: $('#dateTo').val(),
        sort_order: $('#sortOrder').val(),
        sort_column: $('#sortColumn').val(),
        limit: $('#limit').val(),
        search: $('#keywordSearch').val(),
        page: page
    };

    try {
        // Показываем индикатор загрузки
        $('#issDataBody').html(`
            <tr>
                <td colspan="8" class="text-center py-4">
                    <div class="loading-spinner mx-auto"></div>
                    <div class="mt-2">Загрузка данных...</div>
                </td>
            </tr>
        `);

        // Загрузка данных
        const response = await fetch('/api/iss/data?' + new URLSearchParams(filters));
        const data = await response.json();

        updateTable(data.data);
        updateCharts(data.charts);
        updateMetrics(data.metrics);
        updatePagination(data.pagination);
    } catch (error) {
        console.error('Ошибка загрузки данных:', error);
        $('#issDataBody').html(`
            <tr>
                <td colspan="8" class="text-center text-danger py-4">
                    <i class="fas fa-exclamation-triangle me-2"></i>
                    Ошибка загрузки данных
                </td>
            </tr>
        `);
    }
}

// Обновление таблицы
function updateTable(data) {
    const tbody = $('#issDataBody');
    tbody.empty();

    if (data.length === 0) {
        tbody.html(`
            <tr>
                <td colspan="8" class="text-center text-muted py-4">
                    <i class="fas fa-database me-2"></i>
                    Нет данных для отображения
                </td>
            </tr>
        `);
        return;
    }

    data.forEach((item, index) => {
        const status = getStatus(item);
        const row = `
            <tr class="searchable animate-slide-left" style="animation-delay: ${index * 0.05}s">
                <td>${new Date(item.timestamp).toLocaleString('ru-RU')}</td>
                <td>${item.latitude.toFixed(4)}</td>
                <td>${item.longitude.toFixed(4)}</td>
                <td>
                    <span class="badge ${getAltitudeClass(item.altitude)}">
                        ${item.altitude.toFixed(2)}
                    </span>
                </td>
                <td>${item.velocity.toFixed(2)}</td>
                <td>${item.temperature ? item.temperature.toFixed(1) : '-'}</td>
                <td>
                    <span class="badge bg-${status.color}">
                        <i class="fas fa-${status.icon} me-1"></i>${status.text}
                    </span>
                </td>
                <td>
                    <button class="btn btn-sm btn-outline-info" onclick="showDetails(${item.id})">
                        <i class="fas fa-eye"></i>
                    </button>
                    <button class="btn btn-sm btn-outline-primary" onclick="exportRecord(${item.id})">
                        <i class="fas fa-download"></i>
                    </button>
                </td>
            </tr>
        `;
        tbody.append(row);
    });
}

// Обновление графиков
function updateCharts(chartData) {
    if (temperatureChart && altitudeChart) {
        temperatureChart.data.labels = chartData.temperature.labels;
        temperatureChart.data.datasets[0].data = chartData.temperature.data;
        temperatureChart.update();
        
        altitudeChart.data.labels = chartData.altitude.labels;
        altitudeChart.data.datasets[0].data = chartData.altitude.data;
        altitudeChart.update();
    }
}

// Обновление метрик
function updateMetrics(metrics) {
    $('#totalRecords').text(metrics.total_records || 0);
    $('#avgAltitude').text(metrics.avg_altitude ? metrics.avg_altitude.toFixed(2) : '0');
    $('#maxVelocity').text(metrics.max_velocity ? metrics.max_velocity.toFixed(2) : '0');
    $('#lastUpdate').text(metrics.last_update ? 
        new Date(metrics.last_update).toLocaleTimeString('ru-RU') : '-');
}

// Обновление пагинации
function updatePagination(pagination) {
    const paginationEl = $('#pagination');
    paginationEl.empty();

    if (pagination.total_pages <= 1) return;

    // Предыдущая страница
    if (pagination.current_page > 1) {
        paginationEl.append(`
            <li class="page-item">
                <button class="page-link" onclick="loadData(${pagination.current_page - 1})">
                    <i class="fas fa-chevron-left"></i>
                </button>
            </li>
        `);
    }

    // Страницы
    for (let i = 1; i <= pagination.total_pages; i++) {
        const active = i === pagination.current_page ? 'active' : '';
        paginationEl.append(`
            <li class="page-item ${active}">
                <button class="page-link" onclick="loadData(${i})">${i}</button>
            </li>
        `);
    }

    // Следующая страница
    if (pagination.current_page < pagination.total_pages) {
        paginationEl.append(`
            <li class="page-item">
                <button class="page-link" onclick="loadData(${pagination.current_page + 1})">
                    <i class="fas fa-chevron-right"></i>
                </button>
            </li>
        `);
    }
}

// Вспомогательные функции
function getAltitudeClass(altitude) {
    if (altitude < 400) return 'bg-danger';
    if (altitude < 410) return 'bg-warning';
    return 'bg-success';
}

function getStatus(item) {
    if (item.velocity > 28000) return { color: 'danger', icon: 'exclamation-triangle', text: 'Высокая скорость' };
    if (item.altitude < 400) return { color: 'warning', icon: 'exclamation-circle', text: 'Низкая орбита' };
    return { color: 'success', icon: 'check-circle', text: 'Норма' };
}

// Экспорт данных
$('#exportCSV').click(async function() {
    const filters = {
        date_from: $('#dateFrom').val(),
        date_to: $('#dateTo').val(),
        format: 'csv'
    };
    
    window.open('/api/iss/export?' + new URLSearchParams(filters), '_blank');
});

$('#exportJSON').click(async function() {
    const filters = {
        date_from: $('#dateFrom').val(),
        date_to: $('#dateTo').val(),
        format: 'json'
    };
    
    window.open('/api/iss/export?' + new URLSearchParams(filters), '_blank');
});

// Сброс фильтров
$('#resetFilters').click(function() {
    $('#dashboardFilters')[0].reset();
    loadData(1);
});

// Применение фильтров
$('#applyFilters').click(() => loadData(1));

// Инициализация при загрузке страницы
$(document).ready(function() {
    initCharts();
    loadData(1);
    
    // Автообновление каждые 30 секунд
    setInterval(() => loadData(1), 30000);
});
</script>
@endpush
@endsection