@extends('layouts.app')

@section('content')
<div class="container py-4">
  <h2>{{ $title }}</h2>  <!-- Используем экранирование по умолчанию -->
  <div class="cms-content">
    {!! Purifier::clean($html) !!}  <!-- Безопасное отображение HTML -->
  </div>
</div>
@endsection