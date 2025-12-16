<!doctype html>
<html lang="ru">
<head>
  <meta charset="utf-8">
  <meta name="viewport" content="width=device-width, initial-scale=1">
  <title>Space Dashboard - Cassiopeia</title>
  
  <!-- Bootstrap CSS -->
  <link href="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/css/bootstrap.min.css" rel="stylesheet">
  
  <!-- Анимации -->
  <link rel="stylesheet" href="{{ asset('css/animations.css') }}">
  
  <!-- Иконки Font Awesome -->
  <link rel="stylesheet" href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.4.0/css/all.min.css">
  
  <!-- Leaflet для карт -->
  <link rel="stylesheet" href="https://unpkg.com/leaflet@1.9.4/dist/leaflet.css"/>
  
  <!-- Chart.js -->
  <script src="https://cdn.jsdelivr.net/npm/chart.js"></script>
  
  <!-- DataTables для фильтрации -->
  <link rel="stylesheet" type="text/css" href="https://cdn.datatables.net/1.13.6/css/jquery.dataTables.min.css">
  
  <style>
    body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
        font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
    }
    
    .navbar-brand {
        font-weight: bold;
        color: #fff !important;
        font-size: 1.5rem;
    }
    
    .card {
        border: none;
        border-radius: 15px;
        box-shadow: 0 5px 15px rgba(0,0,0,0.1);
        transition: all 0.3s ease;
    }
    
    .card:hover {
        transform: translateY(-5px);
        box-shadow: 0 10px 25px rgba(0,0,0,0.15);
    }
    
    .metric-card {
        background: rgba(255, 255, 255, 0.9);
        backdrop-filter: blur(10px);
        border-radius: 12px;
        padding: 20px;
        text-align: center;
    }
    
    .metric-value {
        font-size: 2rem;
        font-weight: bold;
        color: #333;
    }
    
    .metric-label {
        font-size: 0.9rem;
        color: #666;
        text-transform: uppercase;
        letter-spacing: 1px;
    }
    
    /* Кастомный скроллбар */
    ::-webkit-scrollbar {
        width: 8px;
        height: 8px;
    }
    
    ::-webkit-scrollbar-track {
        background: #f1f1f1;
        border-radius: 10px;
    }
    
    ::-webkit-scrollbar-thumb {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 10px;
    }
    
    ::-webkit-scrollbar-thumb:hover {
        background: linear-gradient(135deg, #764ba2 0%, #667eea 100%);
    }
    
    /* Эффект стеклянного морфинга */
    .glass-effect {
        background: rgba(255, 255, 255, 0.15);
        backdrop-filter: blur(10px);
        border: 1px solid rgba(255, 255, 255, 0.2);
    }
  </style>
</head>
<body>
<nav class="navbar navbar-expand-lg navbar-dark glass-effect">
  <div class="container">
    <a class="navbar-brand animate-float" href="/dashboard">
      <i class="fas fa-satellite me-2"></i>Cassiopeia
    </a>
    
    <button class="navbar-toggler" type="button" data-bs-toggle="collapse" data-bs-target="#navbarNav">
      <span class="navbar-toggler-icon"></span>
    </button>
    
    <div class="collapse navbar-collapse" id="navbarNav">
      <ul class="navbar-nav me-auto">
        <li class="nav-item">
          <a class="nav-link @if(Request::is('dashboard')) active @endif" href="/dashboard">
            <i class="fas fa-chart-line me-1"></i>Дашборд
          </a>
        </li>
        <li class="nav-item">
          <a class="nav-link @if(Request::is('iss')) active @endif" href="/iss">
            <i class="fas fa-satellite me-1"></i>МКС
          </a>
        </li>
        <li class="nav-item">
          <a class="nav-link @if(Request::is('osdr')) active @endif" href="/osdr">
            <i class="fas fa-rocket me-1"></i>OSDR
          </a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="/api/jwst/feed" target="_blank">
            <i class="fas fa-hubble me-1"></i>JWST
          </a>
        </li>
        <li class="nav-item">
          <a class="nav-link" href="/api/astro/events" target="_blank">
            <i class="fas fa-star me-1"></i>Астрономия
          </a>
        </li>
      </ul>
      
      <form class="d-flex" id="globalSearchForm">
        <div class="input-group">
          <input type="text" class="form-control" placeholder="Поиск по данным..." id="globalSearch">
          <button class="btn btn-outline-light" type="submit">
            <i class="fas fa-search"></i>
          </button>
        </div>
      </form>
    </div>
  </div>
</nav>

<main class="container my-4">
  @yield('content')
</main>

<script src="https://code.jquery.com/jquery-3.7.0.min.js"></script>
<script src="https://cdn.jsdelivr.net/npm/bootstrap@5.3.3/dist/js/bootstrap.bundle.min.js"></script>
<script src="https://unpkg.com/leaflet@1.9.4/dist/leaflet.js"></script>
<script src="https://cdn.datatables.net/1.13.6/js/jquery.dataTables.min.js"></script>

<script>
// Анимация элементов при скролле
document.addEventListener('DOMContentLoaded', function() {
    // Добавляем анимации всем карточкам
    document.querySelectorAll('.card').forEach((card, index) => {
        card.style.animationDelay = `${index * 0.1}s`;
        card.classList.add('animate-fade-in');
    });
    
    // Анимация строк таблицы
    document.querySelectorAll('tbody tr').forEach((row, index) => {
        row.style.animationDelay = `${index * 0.05}s`;
        row.classList.add('animate-slide-left');
    });
    
    // Глобальный поиск
    $('#globalSearchForm').on('submit', function(e) {
        e.preventDefault();
        const query = $('#globalSearch').val().toLowerCase();
        
        if (query) {
            // Поиск по всей странице
            $('.searchable').each(function() {
                const text = $(this).text().toLowerCase();
                const isVisible = text.includes(query);
                $(this).closest('.card, tr').toggle(isVisible);
                
                if (isVisible) {
                    $(this).closest('.card, tr').addClass('animate-pulse');
                    setTimeout(() => {
                        $(this).closest('.card, tr').removeClass('animate-pulse');
                    }, 1000);
                }
            });
        } else {
            $('.card, tr').show();
        }
    });
    
    // Анимация при наведении
    $('.card-hover').hover(
        function() { $(this).addClass('card-hover'); },
        function() { $(this).removeClass('card-hover'); }
    );
});
</script>

@yield('scripts')
</body>
</html>