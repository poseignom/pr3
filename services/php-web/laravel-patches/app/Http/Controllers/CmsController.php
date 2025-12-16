<?php
namespace App\Http\Controllers;

use App\Repositories\CmsRepository;
use Illuminate\Http\Request;

class CmsController extends Controller {
    
    private CmsRepository $cmsRepo;
    
    public function __construct(CmsRepository $cmsRepo)
    {
        $this->cmsRepo = $cmsRepo;
    }
    
    public function page(string $slug)
    {
        $page = $this->cmsRepo->getPageBySlug($slug);
        
        if (!$page) {
            abort(404, 'Страница не найдена');
        }
        
        return view('cms.page', [
            'title' => $page->title,
            'html' => $page->content
        ]);
    }
}