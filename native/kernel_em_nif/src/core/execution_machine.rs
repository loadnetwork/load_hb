use crate::core::constants::MAX_MEMORY_ALLOCATION;
use bytemuck::{Pod, Zeroable};
use serde_json::Value;
use std::borrow::Cow;
use wgpu::{Adapter, util::DeviceExt};

use super::adapter::AdapterInfoRes;

pub struct KernelExecutor {
    pub device: wgpu::Device,
    pub queue: wgpu::Queue,
}

impl KernelExecutor {
    /// create a new KernelExecutor
    pub async fn new() -> Self {
        // initialize wgpu instance
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());

        // find a suitable GPU
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: None,
                force_fallback_adapter: false,
            })
            .await
            .expect("Failed to find an appropriate adapter");

        println!("Using adapter: {:?}", adapter.get_info());

        // Create device and queue
        let (device, queue) = adapter
            .request_device(&wgpu::DeviceDescriptor {
                label: Some("Kernel Executor Device"),
                required_features: wgpu::Features::empty(),
                required_limits: wgpu::Limits::default(),
                memory_hints: wgpu::MemoryHints::MemoryUsage,
                trace: wgpu::Trace::Off,
            })
            .await
            .expect("Failed to create device");

        Self { device, queue }
    }
    pub async fn get_adapter_info() -> String {
        // initialize wgpu instance
        let instance = wgpu::Instance::new(&wgpu::InstanceDescriptor::default());

        // find a suitable GPU
        let adapter = instance
            .request_adapter(&wgpu::RequestAdapterOptions {
                power_preference: wgpu::PowerPreference::HighPerformance,
                compatible_surface: None,
                force_fallback_adapter: false,
            })
            .await
            .expect("Failed to find an appropriate adapter")
            .get_info();

        let adapter_info = AdapterInfoRes::from(adapter);
        serde_json::to_string(&adapter_info).unwrap()
    }
    /// execute wgsl kernel fns with default options
    pub fn execute_kernel_default(
        &self,
        kernel_code: &str,
        input_data: &[u8],
        output_size_hint: Option<u64>,
    ) -> Vec<u8> {
        // default to homogeneous input/output size
        let output_size = output_size_hint.unwrap_or(input_data.len() as u64);
        let output_size = output_size.min(MAX_MEMORY_ALLOCATION);
        // default to 256
        let workgroup_size = (256, 1, 1);

        // calculates number of workgroups needed to cover all output data
        // assumes 4 bytes (int) per memory address
        let elements = (output_size as f32 / 4.0).ceil() as u32;
        let num_workgroups = (elements + workgroup_size.0 - 1) / workgroup_size.0;
        let dispatch_size = (num_workgroups, 1, 1);

        self.execute_kernel(
            kernel_code,
            "main",
            input_data,
            output_size,
            workgroup_size,
            dispatch_size,
        )
    }

    /// Execute a wsgl kernel code with input and output buffers
    pub fn execute_kernel(
        &self,
        kernel_code: &str,
        entry_point: &str,
        input_data: &[u8],
        output_size: u64,
        workgroup_size: (u32, u32, u32),
        dispatch_size: (u32, u32, u32),
    ) -> Vec<u8> {
        println!("Executing kernel with entry point: {}", entry_point);
        println!(
            "Workgroup size: {:?}, Dispatch size: {:?}",
            workgroup_size, dispatch_size
        );
        println!(
            "Input data size: {} bytes, Output size: {} bytes",
            input_data.len(),
            output_size
        );

        let shader_module = self
            .device
            .create_shader_module(wgpu::ShaderModuleDescriptor {
                label: Some("Dynamic kernel"),
                source: wgpu::ShaderSource::Wgsl(Cow::Borrowed(kernel_code)),
            });

        let input_buffer = self
            .device
            .create_buffer_init(&wgpu::util::BufferInitDescriptor {
                label: Some("Input Buffer"),
                contents: input_data,
                usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_DST,
            });

        let output_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Output Buffer"),
            size: output_size,
            usage: wgpu::BufferUsages::STORAGE | wgpu::BufferUsages::COPY_SRC,
            mapped_at_creation: false,
        });

        let download_buffer = self.device.create_buffer(&wgpu::BufferDescriptor {
            label: Some("Download Buffer"),
            size: output_size,
            usage: wgpu::BufferUsages::MAP_READ | wgpu::BufferUsages::COPY_DST,
            mapped_at_creation: false,
        });

        let bind_group_layout =
            self.device
                .create_bind_group_layout(&wgpu::BindGroupLayoutDescriptor {
                    label: Some("Bind Group Layout"),
                    entries: &[
                        wgpu::BindGroupLayoutEntry {
                            binding: 0,
                            visibility: wgpu::ShaderStages::COMPUTE,
                            ty: wgpu::BindingType::Buffer {
                                ty: wgpu::BufferBindingType::Storage { read_only: true },
                                has_dynamic_offset: false,
                                min_binding_size: None,
                            },
                            count: None,
                        },
                        wgpu::BindGroupLayoutEntry {
                            binding: 1,
                            visibility: wgpu::ShaderStages::COMPUTE,
                            ty: wgpu::BindingType::Buffer {
                                ty: wgpu::BufferBindingType::Storage { read_only: false },
                                has_dynamic_offset: false,
                                min_binding_size: None,
                            },
                            count: None,
                        },
                    ],
                });

        let pipeline_layout = self
            .device
            .create_pipeline_layout(&wgpu::PipelineLayoutDescriptor {
                label: Some("Pipeline Layout"),
                bind_group_layouts: &[&bind_group_layout],
                push_constant_ranges: &[],
            });

        let compute_pipeline =
            self.device
                .create_compute_pipeline(&wgpu::ComputePipelineDescriptor {
                    label: Some("Compute Pipeline"),
                    layout: Some(&pipeline_layout),
                    module: &shader_module,
                    entry_point: Some(entry_point),
                    compilation_options: wgpu::PipelineCompilationOptions::default(),
                    cache: None,
                });

        let bind_group = self.device.create_bind_group(&wgpu::BindGroupDescriptor {
            label: Some("Bind Group"),
            layout: &bind_group_layout,
            entries: &[
                wgpu::BindGroupEntry {
                    binding: 0,
                    resource: input_buffer.as_entire_binding(),
                },
                wgpu::BindGroupEntry {
                    binding: 1,
                    resource: output_buffer.as_entire_binding(),
                },
            ],
        });

        // Create command encoder
        let mut encoder = self
            .device
            .create_command_encoder(&wgpu::CommandEncoderDescriptor {
                label: Some("Command Encoder"),
            });

        // Begin compute pass
        {
            let mut compute_pass = encoder.begin_compute_pass(&wgpu::ComputePassDescriptor {
                label: Some("Compute Pass"),
                timestamp_writes: None,
            });

            compute_pass.set_pipeline(&compute_pipeline);
            compute_pass.set_bind_group(0, &bind_group, &[]);
            compute_pass.dispatch_workgroups(dispatch_size.0, dispatch_size.1, dispatch_size.2);
        }

        encoder.copy_buffer_to_buffer(&output_buffer, 0, &download_buffer, 0, output_size);

        self.queue.submit([encoder.finish()]);

        let buffer_slice = download_buffer.slice(..);
        let (sender, receiver) = futures_intrusive::channel::shared::oneshot_channel();
        buffer_slice.map_async(wgpu::MapMode::Read, move |result| {
            sender.send(result).unwrap();
        });

        // wait for GPU to complete the computation
        let _ = self.device.poll(wgpu::PollType::Wait);

        match pollster::block_on(receiver.receive()) {
            Some(Ok(_)) => {
                let data = buffer_slice.get_mapped_range();
                let result = data.to_vec();
                drop(data);
                download_buffer.unmap();
                result
            }
            _ => {
                panic!("Failed to map buffer");
            }
        }
    }
}
