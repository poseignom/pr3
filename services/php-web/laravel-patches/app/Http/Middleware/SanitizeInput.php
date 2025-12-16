<?php

namespace App\Http\Middleware;

use Closure;
use Illuminate\Http\Request;
use Symfony\Component\HttpFoundation\Response;

class SanitizeInput
{
    public function handle(Request $request, Closure $next): Response
    {
        $input = $request->all();
        
        if (isset($input['html']) || isset($input['content'])) {
            // Очищаем HTML от опасных тегов
            $cleaner = new \HTMLPurifier();
            
            if (isset($input['html'])) {
                $input['html'] = $cleaner->purify($input['html']);
            }
            
            if (isset($input['content'])) {
                $input['content'] = $cleaner->purify($input['content']);
            }
            
            $request->merge($input);
        }
        
        return $next($request);
    }
}