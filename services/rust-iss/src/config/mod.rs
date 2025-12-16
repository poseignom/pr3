use std::env;

#[derive(Debug, Clone)]
pub struct Config {
    pub database_url: String,
    pub redis_url: String,
    pub nasa_api_key: String,
    pub nasa_api_url: String,
    pub iss_api_url: String,
    pub rate_limit_requests: u32,
    pub rate_limit_period: u64,
}

impl Config {
    pub fn from_env() -> Self {
        dotenvy::dotenv().ok();
        
        Config {
            database_url: env::var("DATABASE_URL")
                .expect("DATABASE_URL must be set"),
            redis_url: env::var("REDIS_URL")
                .unwrap_or_else(|_| "redis://localhost:6379".to_string()),
            nasa_api_key: env::var("NASA_API_KEY")
                .unwrap_or_default(),
            nasa_api_url: env::var("NASA_API_URL")
                .unwrap_or_else(|_| "https://visualization.osdr.nasa.gov/biodata/api/v2/datasets/?format=json".to_string()),
            iss_api_url: env::var("WHERE_ISS_URL")
                .unwrap_or_else(|_| "https://api.wheretheiss.at/v1/satellites/25544".to_string()),
            rate_limit_requests: env::var("RATE_LIMIT_REQUESTS")
                .unwrap_or_else(|_| "100".to_string())
                .parse()
                .unwrap_or(100),
            rate_limit_period: env::var("RATE_LIMIT_PERIOD")
                .unwrap_or_else(|_| "60".to_string())
                .parse()
                .unwrap_or(60),
        }
    }
}