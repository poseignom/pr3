<?php

namespace App\Http\Controllers\Api;

use App\Http\Controllers\Controller;
use Illuminate\Http\Request;
use Illuminate\Support\Facades\DB;
use Illuminate\Support\Facades\Cache;

class IssDataController extends Controller
{
    public function getData(Request $request)
    {
        $request->validate([
            'date_from' => 'nullable|date',
            'date_to' => 'nullable|date|after_or_equal:date_from',
            'sort_order' => 'in:asc,desc',
            'sort_column' => 'string',
            'limit' => 'integer|min:1|max:1000',
            'search' => 'nullable|string|max:100',
            'page' => 'integer|min:1'
        ]);
        
        $cacheKey = 'iss_data_' . md5(json_encode($request->all()));
        
        // Кэшируем на 30 секунд
        $data = Cache::remember($cacheKey, 30, function () use ($request) {
            return $this->fetchData($request);
        });
        
        return response()->json($data);
    }
    
    private function fetchData(Request $request)
    {
        $query = DB::table('iss_fetch_log')
            ->select(
                'id',
                'fetched_at as timestamp',
                DB::raw("(payload->>'latitude')::float as latitude"),
                DB::raw("(payload->>'longitude')::float as longitude"),
                DB::raw("(payload->>'altitude')::float as altitude"),
                DB::raw("(payload->>'velocity')::float as velocity")
            );
        
        // Фильтры по дате
        if ($request->filled('date_from')) {
            $query->where('fetched_at', '>=', $request->date_from);
        }
        
        if ($request->filled('date_to')) {
            $query->where('fetched_at', '<=', $request->date_to);
        }
        
        // Поиск
        if ($request->filled('search')) {
            $search = $request->search;
            $query->where(function($q) use ($search) {
                $q->whereRaw("payload::text like ?", ["%{$search}%"]);
            });
        }
        
        // Сортировка
        $sortColumn = $request->get('sort_column', 'fetched_at');
        $sortOrder = $request->get('sort_order', 'desc');
        $query->orderBy($sortColumn, $sortOrder);
        
        // Пагинация
        $perPage = $request->get('limit', 25);
        $page = $request->get('page', 1);
        $offset = ($page - 1) * $perPage;
        
        $total = $query->count();
        $data = $query->offset($offset)->limit($perPage)->get();
        
        // Графики
        $charts = $this->prepareCharts($data);
        
        // Метрики
        $metrics = $this->calculateMetrics($data);
        
        return [
            'data' => $data,
            'charts' => $charts,
            'metrics' => $metrics,
            'pagination' => [
                'current_page' => $page,
                'per_page' => $perPage,
                'total' => $total,
                'total_pages' => ceil($total / $perPage)
            ]
        ];
    }
    
    private function prepareCharts($data)
    {
        $temperatureData = [];
        $altitudeData = [];
        $labels = [];
        
        foreach ($data as $item) {
            $labels[] = $item->timestamp;
            $temperatureData[] = rand(15, 25); // Заглушка
            $altitudeData[] = $item->altitude;
        }
        
        return [
            'temperature' => [
                'labels' => $labels,
                'data' => $temperatureData
            ],
            'altitude' => [
                'labels' => $labels,
                'data' => $altitudeData
            ]
        ];
    }
    
    private function calculateMetrics($data)
    {
        if ($data->isEmpty()) {
            return [
                'total_records' => 0,
                'avg_altitude' => 0,
                'max_velocity' => 0,
                'last_update' => null
            ];
        }
        
        $altitudes = $data->pluck('altitude')->filter();
        $velocities = $data->pluck('velocity')->filter();
        
        return [
            'total_records' => $data->count(),
            'avg_altitude' => $altitudes->avg(),
            'max_velocity' => $velocities->max(),
            'last_update' => $data->first()->timestamp
        ];
    }
    
    public function export(Request $request)
    {
        $request->validate([
            'date_from' => 'nullable|date',
            'date_to' => 'nullable|date',
            'format' => 'in:csv,json'
        ]);
        
        $data = $this->fetchData($request);
        
        if ($request->format === 'csv') {
            return $this->exportToCsv($data['data']);
        }
        
        return response()->json($data['data']);
    }
    
    private function exportToCsv($data)
    {
        $filename = 'iss_data_' . date('Y-m-d_H-i-s') . '.csv';
        $headers = [
            'Content-Type' => 'text/csv',
            'Content-Disposition' => 'attachment; filename="' . $filename . '"',
        ];
        
        $callback = function() use ($data) {
            $file = fopen('php://output', 'w');
            
            // Заголовок CSV
            fputcsv($file, [
                'Timestamp', 'Latitude', 'Longitude', 
                'Altitude (km)', 'Velocity (km/h)', 'Temperature (°C)'
            ]);
            
            // Данные
            foreach ($data as $row) {
                fputcsv($file, [
                    $row->timestamp,
                    $row->latitude,
                    $row->longitude,
                    $row->altitude,
                    $row->velocity,
                    $row->temperature ?? ''
                ]);
            }
            
            fclose($file);
        };
        
        return response()->stream($callback, 200, $headers);
    }
}