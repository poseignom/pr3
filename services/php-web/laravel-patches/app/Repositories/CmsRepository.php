<?php

namespace App\Repositories;

use Illuminate\Support\Facades\DB;

class CmsRepository
{
    public function getPageBySlug(string $slug): ?object
    {
        // Безопасный параметризованный запрос
        return DB::selectOne("
            SELECT title, body as content 
            FROM cms_pages 
            WHERE slug = ? 
            LIMIT 1
        ", [$slug]);
    }
    
    public function getDashboardBlock(): ?object
    {
        return DB::selectOne("
            SELECT content 
            FROM cms_blocks 
            WHERE slug = 'dashboard_experiment' 
            AND is_active = TRUE 
            LIMIT 1
        ");
    }
}