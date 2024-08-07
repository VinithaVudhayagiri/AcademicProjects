BEGIN;

DROP TABLE IF EXISTS public.fact_sales_daily;
DROP TABLE IF EXISTS public.sale;
DROP TABLE IF EXISTS public.customer;
DROP TABLE IF EXISTS public.employee;
DROP TABLE IF EXISTS public.store;
DROP TABLE IF EXISTS public.seller;
DROP TABLE IF EXISTS public.product;
DROP TABLE IF EXISTS public.orders;

CREATE TABLE IF NOT EXISTS public.customer
(
    customer_id character varying COLLATE pg_catalog."default" NOT NULL,
    first_name character varying COLLATE pg_catalog."default" NOT NULL,
    last_name character varying COLLATE pg_catalog."default" NOT NULL,
    email character varying COLLATE pg_catalog."default" NOT NULL,
    address character varying COLLATE pg_catalog."default" NOT NULL,
    phone_number character varying COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT customer_pkey PRIMARY KEY (customer_id)
);

CREATE TABLE IF NOT EXISTS public.employee
(
    employee_id character varying COLLATE pg_catalog."default" NOT NULL,
    first_name character varying COLLATE pg_catalog."default" NOT NULL,
    last_name character varying COLLATE pg_catalog."default" NOT NULL,
    username character varying COLLATE pg_catalog."default" NOT NULL,
    address character varying COLLATE pg_catalog."default" NOT NULL,
    salary numeric NOT NULL,
    CONSTRAINT employee_pkey PRIMARY KEY (employee_id)
);

CREATE TABLE IF NOT EXISTS public.fact_sales_daily
(
    drug_id character varying COLLATE pg_catalog."default" NOT NULL,
    sale_id character varying COLLATE pg_catalog."default" NOT NULL,
    store_id character varying COLLATE pg_catalog."default" NOT NULL,
    seller_id character varying COLLATE pg_catalog."default" NOT NULL,
    product_id character varying COLLATE pg_catalog."default" NOT NULL,
    order_id character varying COLLATE pg_catalog."default" NOT NULL,
    employee_id character varying COLLATE pg_catalog."default" NOT NULL,
    customer_id character varying COLLATE pg_catalog."default" NOT NULL,
    order_date date NOT NULL,
    price numeric NOT NULL,
    CONSTRAINT fact_sales_daily_pkey PRIMARY KEY (drug_id)
);

CREATE TABLE IF NOT EXISTS public.orders
(
    order_id character varying COLLATE pg_catalog."default" NOT NULL,
    type_of_order character varying COLLATE pg_catalog."default" NOT NULL,
    payment_method character varying COLLATE pg_catalog."default" NOT NULL,
    quantity integer NOT NULL,
    mode_of_delivery character varying COLLATE pg_catalog."default" NOT NULL,
    delivery_partner character varying COLLATE pg_catalog."default",
    CONSTRAINT orders_pkey PRIMARY KEY (order_id)
);

CREATE TABLE IF NOT EXISTS public.product
(
    product_id character varying COLLATE pg_catalog."default" NOT NULL,
    name character varying COLLATE pg_catalog."default" NOT NULL,
    brand character varying COLLATE pg_catalog."default" NOT NULL,
    price numeric NOT NULL,
    qty_left_in_stock integer NOT NULL,
    manf_date date NOT NULL,
    exp_date date NOT NULL,
    manufacturer character varying COLLATE pg_catalog."default" NOT NULL,
    disease character varying COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT product_pkey PRIMARY KEY (product_id)
);

CREATE TABLE IF NOT EXISTS public.sale
(
    sale_date date NOT NULL,
    sale_id character varying COLLATE pg_catalog."default" NOT NULL,
    order_id character varying COLLATE pg_catalog."default" NOT NULL,
    product_id character varying COLLATE pg_catalog."default" NOT NULL,
    price numeric NOT NULL,
    CONSTRAINT sale_pkey PRIMARY KEY (sale_id)
);

CREATE TABLE IF NOT EXISTS public.seller
(
    seller_id character varying COLLATE pg_catalog."default" NOT NULL,
    seller_name character varying COLLATE pg_catalog."default" NOT NULL,
    address character varying COLLATE pg_catalog."default" NOT NULL,
    phone_number character varying COLLATE pg_catalog."default" NOT NULL,
    email character varying COLLATE pg_catalog."default" NOT NULL,
    joined_year integer NOT NULL,
    CONSTRAINT seller_pkey PRIMARY KEY (seller_id)
);

CREATE TABLE IF NOT EXISTS public.store
(
    store_id character varying COLLATE pg_catalog."default" NOT NULL,
    store_name character varying COLLATE pg_catalog."default" NOT NULL,
    address character varying COLLATE pg_catalog."default" NOT NULL,
    city character varying COLLATE pg_catalog."default" NOT NULL,
    country character varying COLLATE pg_catalog."default" NOT NULL,
    phone_number character varying COLLATE pg_catalog."default" NOT NULL,
    CONSTRAINT store_pkey PRIMARY KEY (store_id)
);

ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_customer_id_fkey FOREIGN KEY (customer_id)
    REFERENCES public.customer (customer_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_employee_id_fkey FOREIGN KEY (employee_id)
    REFERENCES public.employee (employee_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_order_id_fkey FOREIGN KEY (order_id)
    REFERENCES public.orders (order_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_product_id_fkey FOREIGN KEY (product_id)
    REFERENCES public.product (product_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_sale_id_fkey FOREIGN KEY (sale_id)
    REFERENCES public.sale (sale_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_seller_id_fkey FOREIGN KEY (seller_id)
    REFERENCES public.seller (seller_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.fact_sales_daily
    ADD CONSTRAINT fact_sales_daily_store_id_fkey FOREIGN KEY (store_id)
    REFERENCES public.store (store_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.sale
    ADD CONSTRAINT sale_order_id_fkey FOREIGN KEY (order_id)
    REFERENCES public.orders (order_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;


ALTER TABLE IF EXISTS public.sale
    ADD CONSTRAINT sale_product_id_fkey FOREIGN KEY (product_id)
    REFERENCES public.product (product_id) MATCH SIMPLE
    ON UPDATE NO ACTION
    ON DELETE NO ACTION;

END;