use sqlx::{PgPool, postgres::PgPoolOptions};
use crate::domain::iss::IssFetchLog;

pub struct Database {
    pool: PgPool,
}

impl Database {
    pub async fn new(database_url: &str) -> Result<Self, sqlx::Error> {
        let pool = PgPoolOptions::new()
            .max_connections(5)
            .connect(database_url)
            .await?;
            
        Ok(Database { pool })
    }
    
    pub async fn init_tables(&self) -> Result<(), sqlx::Error> {
        sqlx::query(
            r#"
            CREATE TABLE IF NOT EXISTS iss_fetch_log (
                id BIGSERIAL PRIMARY KEY,
                fetched_at TIMESTAMPTZ NOT NULL DEFAULT now(),
                source_url TEXT NOT NULL,
                payload JSONB NOT NULL
            )
            "#
        ).execute(&self.pool).await?;
        
        // Остальные таблицы...
        
        Ok(())
    }
    
    pub async fn get_last_iss_position(&self) -> Result<Option<IssFetchLog>, sqlx::Error> {
        sqlx::query_as(
            "SELECT id, fetched_at, source_url, payload FROM iss_fetch_log ORDER BY id DESC LIMIT 1"
        ).fetch_optional(&self.pool).await
    }
}